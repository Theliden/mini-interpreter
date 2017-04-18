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

toExp :: [Token] -> Either Exp ParseError
toExp ((Lp prn):rest1) = case toExpList rest1 ExpEmpty of
  Left (ptype, lst, []) -> if ptype == getParenType prn
                              then Left (ExpList ptype lst)
                              else Right ParenMismatch
  Left _ -> Right BadListFormat
  Right er -> Right er
toExp [Id str] = Left (Symbol str)
toExp [TokNumb x] = Left (Numeric x)
toExp _ = Right BadListFormat

data ExpCont = ExpEmpty | Cons Exp ExpCont | MakeList ParenType ExpCont

type Result = (ParenType,[Exp],[Token])

toExpList :: [Token] -> ExpCont -> Either Result ParseError
toExpList [] _ = Right BadListFormat
toExpList ((Rp prn):rest) c = applyExpCont c (getParenType prn,[],rest)
toExpList ((Id str):rest) c = toExpList rest (Cons (Symbol str) c)
toExpList ((TokNumb x):rest) c = toExpList rest (Cons (Numeric x) c)
toExpList ((Lp prn):rest) c = toExpList rest (MakeList (getParenType prn) c)

applyExpCont :: ExpCont -> Result -> Either Result ParseError
applyExpCont ExpEmpty x = Left x
applyExpCont (Cons e c) (ptype,lst,rest) = applyExpCont c (ptype,e:lst,rest)
applyExpCont (MakeList ptype1 c) (ptype2,lst,rest)
  | ptype1==ptype2 = toExpList rest (Cons (ExpList ptype1 lst) c)
  | True = Right ParenMismatch
