
module Main where

import UnParse
import UnSyntax

import Control.Monad.IO.Class   ( liftIO )
import Data.ByteString.Lazy     ( ByteString, getContents, readFile )
import System.Environment       ( getArgs, getProgName )
import System.Exit              ( exitFailure, exitSuccess )

import Prelude hiding ( getContents, readFile )

main :: IO ()
main = do
    args <- getArgs
    runArgs args

parse :: ByteString -> UnEnv Module
parse bs = do
    case parseModule (lexer1 bs) of
        Right mod -> do
            liftIO $ putStrLn "Parse successful"
            liftIO $ putStrLn $ pShow mod
            return mod
        Left e  -> do
            liftIO $ putStrLn e
            liftIO $ exitFailure

type UnEnv a = IO a

runArgs :: [String] -> UnEnv ()
runArgs [] = do
    source <- liftIO $ getContents
    mod <- parse source
--    g <- check mod
    return ()
runArgs ["--help"] = do
    usage
    liftIO exitSuccess
runArgs ("--help" : _) = do
    liftIO $ putStrLn "unexpected args after --help"
    liftIO $ exitFailure
runArgs ["--version"] = do
    version
    liftIO $ exitSuccess
runArgs ("--version" : _) = do
    liftIO $ putStrLn "unexpected args after --version"
    liftIO $ exitFailure
runArgs (f : fs) =
    case f of
        '-':_ -> do
            liftIO $ putStrLn $ "unknown flag: " ++ f
            liftIO $ exitFailure
        f -> do
            source <- liftIO $ readFile f
            mod <- parse source
--            g <- check mod
            moreArgs fs

moreArgs :: [String] -> UnEnv ()
moreArgs [] =
    return ()

usage :: UnEnv ()
usage =
    liftIO $ putStrLn $ unlines [
        "unlang <flag>* [<file>*]",
        "  --help        This usage information.",
        "  --version"
        ]

version :: UnEnv ()
version =
    liftIO $ putStrLn $ "UnLang, version 1.0.0"
