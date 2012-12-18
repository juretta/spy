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

import System.OSX.FSEvents
import System.Console.CmdArgs
import System.Cmd
import System.Exit
import System.IO (stderr, hPrint)
import System.FilePath.GlobPattern
import System.FilePath (splitDirectories, takeFileName)
import Control.Monad (unless)
import Control.Exception (bracket)
import Data.Maybe (fromMaybe, maybeToList)
import Data.Bits
import Data.Word
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
spy config = bracket
  (eventStreamCreate [dir config] 1.0 True True True $ handleEvent config)
  eventStreamDestroy
  (\_ -> getLine)

-- | Handle the FS event based on the current Spy run configuration
handleEvent :: Spy -> Event -> IO ()
handleEvent config@Run{..} event =
        unless (skipEvent config event) $
        runCommand command pathAsArg >>=
            \exit -> case exit of
                ExitSuccess     -> return ()
                ExitFailure i   -> hPrint stderr $ "Failed to execute " ++ command ++ " - exit code: " ++ show i
        where pathAsArg = if notifyOnly then
                            Nothing
                            else Just (eventPath event)

handleEvent config@Watch{..} event =
        unless (skipEvent config event) $
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
outputHandler Json  = \event -> encode $ toJSObject [
    ("path", eventPath event),
    ("id", show $ eventId event),
    ("flags", show $ showEventFlags $ eventFlags event)]
outputHandler Plain = eventPath


-- | Skip events based on the configuration given
skipEvent :: Spy -> Event -> Bool
skipEvent config event = skipHidden || skipNonMatchingGlob
    where skipHidden            = let includeHiddenfiles = hidden config
                                  in not includeHiddenfiles && containsHiddenPathElement path
          skipNonMatchingGlob   = maybe False (not . matchesFile path) $ glob config
          path                  = eventPath event


matchesFile :: FilePath -> GlobPattern -> Bool
matchesFile path glob' = takeFileName path ~~ glob'

containsHiddenPathElement :: FilePath -> Bool
containsHiddenPathElement path = any isHidden paths
                where paths = splitDirectories path
                      isHidden name' = case name' of
                                            (x:_)   -> x == '.'
                                            _       -> False

-- =================================================================================
-- The following code is taken from:
-- https://github.com/luite/hfsevents/blob/master/test/trace.hs
-- Copyright (c) 2012, Luite Stegeman
showEventFlags :: Word64 -> [String]
showEventFlags fl = map fst . filter hasFlag $ flagList
  where hasFlag (_,f) = fl .&. f /= 0


flagList :: [(String, Word64)]
flagList = [ ("MustScanSubDirs"   , 0x00000001)
           , ("UserDropped"       , 0x00000002)
           , ("KernelDropped"     , 0x00000004)
           , ("EventIdsWrapped"   , 0x00000008)
           , ("HistoryDone"       , 0x00000010)
           , ("RootChanged"       , 0x00000020)
           , ("Mount"             , 0x00000040)
           , ("Unmount"           , 0x00000080)
           , ("ItemCreated"       , 0x00000100)
           , ("ItemRemoved"       , 0x00000200)
           , ("ItemInodeMetaMod"  , 0x00000400)
           , ("ItemRenamed"       , 0x00000800)
           , ("ItemModified"      , 0x00001000)
           , ("ItemFinderInfoMod" , 0x00002000)
           , ("ItemChangeOwner"   , 0x00004000)
           , ("ItemXattrMod"      , 0x00008000)
           , ("ItemIsFile"        , 0x00010000)
           , ("ItemIsDir"         , 0x00020000)
           , ("ItemIsSymlink"     , 0x00040000)
           ]
