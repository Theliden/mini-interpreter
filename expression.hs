-- expression.hs
-- Robert Cummings
-- April 2017

-- Expression:
-- An Exp is a structure of nested lists containing strings or integers.
-- The first step in the parsing proccess is to convert a list of tokens to an Exp.
-- It is only later that we will look at the specific syntax of our language.

module Expression where
import Tokenizing

data ParseError = ParenMismatch | BadListFormat | FunArgs | OpArgs deriving Show

data ParenType = Round | Square | Curly | NotAParen deriving (Eq,Show)

getParenType :: Char -> ParenType
getParenType c = case c of
  '(' -> Round ; ')' -> Round
  '[' -> Square ; ']' -> Square
  '{' -> Curly ; '}' -> Curly
  _ -> NotAParen

data Exp = ExpList ParenType [Exp] | Symbol String | Numeric Integer deriving Show

toExp :: [Token] -> Either ParseError Exp
toExp ((Lp prn):rest1) = case toExpList rest1 ExpEmpty of
  Right (ptype, lst, []) -> if ptype == getParenType prn
                              then Right (ExpList ptype lst)
                              else Left ParenMismatch
  Right _ -> Left BadListFormat
  Left er -> Left er
toExp [Id str] = Right (Symbol str)
toExp [TokNumb x] = Right (Numeric x)
toExp _ = Left BadListFormat

data ExpCont = ExpEmpty | Cons Exp ExpCont | MakeList ParenType ExpCont

type Result = (ParenType,[Exp],[Token])

toExpList :: [Token] -> ExpCont -> Either ParseError Result
toExpList [] _ = Left BadListFormat
toExpList ((Rp prn):rest) c = applyExpCont c (getParenType prn,[],rest)
toExpList ((Id str):rest) c = toExpList rest (Cons (Symbol str) c)
toExpList ((TokNumb x):rest) c = toExpList rest (Cons (Numeric x) c)
toExpList ((Lp prn):rest) c = toExpList rest (MakeList (getParenType prn) c)

applyExpCont :: ExpCont -> Result -> Either ParseError Result
applyExpCont ExpEmpty x = Right x
applyExpCont (Cons e c) (ptype,lst,rest) = applyExpCont c (ptype,e:lst,rest)
applyExpCont (MakeList ptype1 c) (ptype2,lst,rest)
  | ptype1==ptype2 = toExpList rest (Cons (ExpList ptype1 lst) c)
  | True = Left ParenMismatch
