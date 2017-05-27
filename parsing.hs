-- parsing.hs
-- Robert Cummings
-- April 2017

-- Parsing:
-- After converting our string to an expression, we construct an AST.

module Parsing(parse,Op(..),Ast(..)) where

import Tokenizing
import Expression

parse :: String -> Either ParseError Ast
parse s = case toExp (tokenize s) of
  Right ex -> toAst ex AstEmpty
  Left er -> Left er

data Op = Plus | Times  deriving Show

data Ast = Number Integer | Bin Op Ast Ast
         | App Ast Ast | Var String
         | Fun String Ast | Define String Ast deriving Show

data AstCont = AstEmpty | AppL Exp AstCont | AppR Ast AstCont
             | BinL Op Exp AstCont | BinR Op Ast AstCont
             | MakeFun String AstCont | MakeDef String AstCont

isBinOp :: String -> Bool
isBinOp s = elem  s ["+","*"]

toAst :: Exp -> AstCont -> Either ParseError Ast
toAst (Numeric x) c = applyAstCont c (Number x)
toAst (Symbol s) c | isBinOp s = Left OpArgs
                   | True = applyAstCont c (Var s)
toAst (ExpList _ [Symbol "+",x,y]) c = toAst x (BinL Plus y c)
toAst (ExpList _ [Symbol "*",x,y]) c = toAst x (BinL Times y c)
toAst (ExpList _ [x,y]) c = toAst x (AppL y c)
toAst (ExpList _ [Symbol "fun",ExpList _ [Symbol x],y]) c = toAst y (MakeFun x c)
toAst (ExpList _ [Symbol "with",ExpList _ [ExpList _ [Symbol x,y]],z]) c
  = toAst z (MakeFun x (AppL y c))
toAst (ExpList _ [Symbol "define",Symbol x,y]) c = toAst y (MakeDef x c)
toAst _ _ = Left FunArgs

applyAstCont :: AstCont -> Ast -> Either ParseError Ast
applyAstCont AstEmpty v = Right v
applyAstCont (AppL e k) v = toAst e (AppR v k)
applyAstCont (BinL op e k) v = toAst e (BinR op v k)
applyAstCont (AppR x k) y = applyAstCont k (App x y)
applyAstCont (BinR op x k) y = applyAstCont k (Bin op x y)
applyAstCont (MakeFun x k) v = applyAstCont k (Fun x v)
applyAstCont (MakeDef x k) v = applyAstCont k (Define x v)
