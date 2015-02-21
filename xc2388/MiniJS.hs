
module Main where

import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )

import LexMiniJS
import ParMiniJS
import SkelMiniJS
import PrintMiniJS
import AbsMiniJS
import Eval

import ErrM

run :: String -> IO ()
run input = 
  let tokens = myLexer input in 
    case pSeq tokens of
       Bad s -> do 
            putStrLn "\nParse failed...\n"
            putStrLn s
       Ok tree -> do 
          putStrLn (evaluate tree)

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> do
              input <- hGetContents stdin
              run input
            [fileName] -> do
              input <- readFile fileName
              run input
            _ -> putStrLn "usage: MiniJS <file.js>"
