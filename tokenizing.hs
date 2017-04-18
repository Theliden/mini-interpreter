-- tokenizing.hs
-- Robert Cummings
-- April 2017

-- Tokenizing:
-- Convert a string to a list of tokens.

module Tokenizing(Token(..),tokenize,leftParen,rightParen) where

import Data.Char

data Token = Lp Char | Rp Char | Id String | TokNumb Integer deriving Show

symbolic :: Char -> Bool
symbolic c = not (isSpace c || elem c "()[]{}'`;#|\\")

numeric :: Char -> Bool
numeric = isDigit

leftParen :: Char -> Bool
leftParen c = elem c ['(','[','{']

rightParen :: Char -> Bool
rightParen c = elem c [')',']','}']

tokenize :: String -> [Token]
tokenize s = tokenizeHelp s []

tokenizeHelp :: String -> [Token] -> [Token]
tokenizeHelp "" acc = reverse acc
tokenizeHelp s@(c:cs) acc
  | leftParen c = tokenizeHelp cs ((Lp c):acc)
  | rightParen c = tokenizeHelp cs ((Rp c):acc)
  | numeric c || c=='-' && not (null cs) && numeric (head cs) =
      let (x,rest) = getNum s in
        tokenizeHelp rest ((TokNumb x):acc)
  | symbolic c =
      let (w,rest) = getWord s in
        tokenizeHelp rest ((Id w):acc)
  | True = tokenizeHelp cs acc

getWord :: String -> (String,String)
getWord "" = ("","")
getWord s@(c:cs) | symbolic c = 
                   let (w,rest) = getWord cs in
                     (c:w,rest)
                 | True = ("",s)

getNum :: String -> (Integer,String)
getNum ('-':s) = let (x,rest) = getNum s in (-x,rest)
getNum s = let helper "" acc = (acc,"")
               helper rest@(c:cs) acc
                 | numeric c = helper cs (10 * acc + toInteger (digitToInt c))
                 | True = (acc,rest)
           in helper s 0

