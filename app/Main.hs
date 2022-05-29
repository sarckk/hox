module Main where

import Control.Monad
import Lib
import System.Environment

import qualified Scanner as S

main :: IO ()
main = do
    args <- getArgs
    runDepending args

runDepending :: [String] -> IO ()
runDepending args
    | length args > 1 = error "Must have 1 argument!"
    | length args == 1 = do
        runFile $ head args
    | otherwise = runPrompt

runFile :: String -> IO ()
runFile filePath = do
    contents <- readFile filePath
    run contents

runPrompt :: IO ()
runPrompt = do
    putStr "> "
    line <- getLine
    unless (null line) $ do
        run line
        runPrompt

run :: String -> IO ()
run source = undefined
