-- main.hs
-- Robert Cummings
-- April 2017

-- Main:
-- This module manages all the IO interaction.

module Main where

import Parsing
import Tokenizing
import Interp
import Data.Char
import System.IO

depthChange :: Char -> Integer
depthChange c
  | leftParen c = 1
  | rightParen c = -1
  | True = 0

nextBalancedString :: IO String
nextBalancedString =
  let helper dep
        = do c <- getChar
             if dep<0 || dep<=0 && (isSpace c)
               then return ""
               else do s <- helper (dep + depthChange c)
                       return (c:s)
  in do helper 0 

main :: IO ()
main = repl emptyState

repl :: State -> IO ()
repl st = do
  putStr ">"
  hFlush stdout
  done <- isEOF
  if done
    then putStrLn "Goodbye!"
    else do s <- nextBalancedString
            case parse s of
              Right er -> do print er ; repl st
              Left (Define x ast)
                -> case interpDefine x ast st of
                     Right er -> do print er ; repl st
                     Left st' -> repl st'
              Left ast
                -> case interpNum ast st of
                     Right er -> do print er ; repl st
                     Left (x,st') -> do print x ; repl st'

  
