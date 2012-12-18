{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import System.Console.CmdArgs
import Spy.Watcher

version :: String
version = "spy v0.3, (C) Stefan Saasen"

hiddenOpts x = x &= help "Set to true if hidden files/directories should be included" &= name "i" &= typ  "BOOL"

watch :: Spy
watch = Watch
    {hidden     = hiddenOpts False
    ,dir        = "."               &= argPos 0  &= typ "FILE/DIR"
    ,format     = Just plainFormat  &= name "f"  &= help "Specify the output format ('json', 'plain')"
    ,glob       = Nothing           &= args &= typ "GLOB"
    }
    &= help "Watch a directory (or file) for file changes"
    &= auto

run :: Spy
run = Run
    {hidden     = hiddenOpts False
    ,command    = def           &= argPos 0                        &= typ "CMD"
    ,dir        = "."           &= argPos 1                        &= typ "FILE/DIR"
    ,glob       = Nothing       &= args                            &= typ "GLOB"
    ,notifyOnly = False         &= name "n"  &= name "notify-only" &= typ "BOOL"
    }
    &= help "Run a command whenever a file changes"


mode :: Mode (CmdArgs Spy)
mode = cmdArgsMode $ modes [watch,run]
        &= help "spy watches for file changes on the file system"
        &= program "spy" &= summary version


main :: IO ()
main = do
    config <- cmdArgsRun mode
    spy config
    putStrLn "No eyes on the target"

