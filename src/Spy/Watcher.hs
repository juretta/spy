{-# LANGUAGE DeriveDataTypeable,RecordWildCards #-}

module Spy.Watcher
(
 spy
,Format
,plainFormat
,Spy(..)
,skipEvent
,matchesFile
,containsHiddenPathElement
) where

import System.FSNotify
import System.Console.CmdArgs
import System.Cmd
import System.Exit
import System.IO (stderr, hPrint)
import System.FilePath.GlobPattern
import System.FilePath (splitDirectories, takeFileName)
import Filesystem.Path.CurrentOS (encodeString, decodeString)
import Data.Time.Clock(UTCTime)
import Data.Maybe (fromMaybe, maybeToList)
import Text.JSON

-- | The output format when Spy prints out changes to STDOUT
data Format = Json | Plain deriving (Show, Eq, Data, Typeable)

-- | Spy run modes.
data Spy = Watch {
     dir                :: FilePath
    ,glob               :: Maybe GlobPattern
    ,format             :: Maybe Format
    ,hidden             :: Bool
} | Run {
     dir                :: FilePath
    ,command            :: String
    ,glob               :: Maybe GlobPattern
    ,hidden             :: Bool
    ,notifyOnly         :: Bool
} deriving (Data,Typeable,Show,Eq)

-- | Return the Plain format.
plainFormat :: Format
plainFormat = Plain

-- | Register for FS events using the given Spy config.
spy :: Spy -> IO String
spy config = withManager $ \wm ->
  watchDir wm (decodeString $ dir config)
              (not . skipEvent config)
              (handleEvent config) >>
  getLine

-- | Handle the FS event based on the current Spy run configuration
handleEvent :: Spy -> Event -> IO ()
handleEvent Run{..} event =
        runCommand command pathAsArg >>=
            \exit -> case exit of
                ExitSuccess     -> return ()
                ExitFailure i   -> hPrint stderr $ "Failed to execute " ++ command ++ " - exit code: " ++ show i
        where pathAsArg = if notifyOnly then
                            Nothing
                            else Just (eventPath event)

handleEvent Watch{..} event =
        putStrLn $ (outputHandler $ fromMaybe Plain format) event

-- =================================================================================

runCommand :: Command -> Maybe FilePath -> IO ExitCode
runCommand cmd maybePath = rawSystem p $ mergeArgs args'
    where (p, args')            = case words cmd of
                                    (x:xs) -> (x, xs)
                                    _      -> ("", [])
          mergeArgs defaultArgs = defaultArgs ++ maybeToList maybePath

-- =================================================================================

type Printer = (Event -> String)
type Command = String

outputHandler :: Format -> Printer
outputHandler Json  = \event -> encode $ makeObj [
    ("path", showJSON $ eventPath event),
    ("time", showJSON . show $ eventTime event)]
outputHandler Plain = eventPath


-- | Skip events based on the configuration given
skipEvent :: Spy -> Event -> Bool
skipEvent config event = skipHidden || skipNonMatchingGlob
    where skipHidden            = let includeHiddenfiles = hidden config
                                  in not includeHiddenfiles && containsHiddenPathElement path
          skipNonMatchingGlob   = maybe False (not . matchesFile path) $ glob config
          path                  = eventPath event

eventTime :: Event -> UTCTime
eventTime (Added _ t) = t
eventTime (Modified _ t) = t
eventTime (Removed _ t) = t

eventPath :: Event -> FilePath
eventPath (Added fp _) = encodeString fp
eventPath (Modified fp _) = encodeString fp
eventPath (Removed fp _) = encodeString fp

matchesFile :: FilePath -> GlobPattern -> Bool
matchesFile path glob' = takeFileName path ~~ glob'

containsHiddenPathElement :: FilePath -> Bool
containsHiddenPathElement path = any isHidden paths
                where paths = splitDirectories path
                      isHidden name' = case name' of
                                            (x:_)   -> x == '.'
                                            _       -> False
