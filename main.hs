-- main.hs
-- Robert Cummings
-- April 2017

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
main = repl []

repl :: Env -> IO ()
repl env
  = do done <- isEOF
       if done
         then putStrLn "Goodbye!"
         else do s <- nextBalancedString
                 case parse s of
                   Right er -> do print er ; repl env
                   Left (Define x ast) -> case interpVal ast env of
                     Right er -> do print er ; repl env
                     Left v -> repl ((x,v):env)
                   Left ast -> case interpNum ast env of
                     Right er -> do print er ; repl env
                     Left x -> do print x ; repl env

  
