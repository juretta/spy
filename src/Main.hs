{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main where

import System.IO (stderr, hPrint)
import System.Console.CmdArgs
import Spy.Watcher

version :: String
version = "spy v0.1, (C) Stefan Saasen"

setup :: Mode (CmdArgs Spy)
setup = cmdArgsMode $ Spy
    {dir        = "."               &= argPos 0  &= typDir
    ,glob       = Nothing           &= args      &= typ "GLOB"
    ,recursive  = True              &= name "r"  &= help "Watch the directory recursively (true, default) not only the top level (false)"
    ,format     = Just plainFormat  &= name "f"  &= help "Specify the output format ('json', 'plain')"
    ,hidden     = False             &= name "i"  &= help "Set to true if hidden files/directories should be included"}
    &= summary version &=
    details ["spy watches for file changes on the file system. If a file modification occurs, spy prints the path to the affected file to STDOUT.",""
            ,"To watch all the files and directories in the current directory:","  spy ."]


main :: IO ()
main = do
    config@Spy{..} <- cmdArgsRun setup
    hPrint stderr $ "Watching " ++ dir
    spy config
    return ()


