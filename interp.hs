-- Copyright 2017, Robert Cummings, All rights reserved.

module Interp(interpNum,interpVal,Env) where

import Parsing

data Val = Numb Integer
         | Closure String Ast Env

type Env = [(String, Val)]

data Cont = MT | AppL Ast Env Cont | AppR Val Cont
          | BinL Op Ast Env Cont | BinR Op Val Cont

opTrans :: Op -> Integer -> Integer -> Integer
opTrans Plus = (+)
opTrans Times = (*)

data InterpError = NotANumber | BadDefine | Undefined | OtherInterpError deriving Show

interpNum :: Ast -> Env -> Either Integer InterpError
interpNum x e = case interpHelp x MT e of
                  Left (Numb y) -> Left y
                  Left _ -> Right NotANumber
                  Right er -> Right er

interpVal :: Ast -> Env -> Either Val InterpError
interpVal x e = interpHelp x MT e

interpHelp :: Ast -> Cont -> Env -> Either Val InterpError
interpHelp (Bin op x y) c e = interpHelp x (BinL op y e c) e
interpHelp (App f x) c e = interpHelp f (AppL x e c) e
interpHelp (Fun fp fb) c e = applyCont c (Closure fp fb e)
interpHelp (Var x) c e = case lookup x e of Just v -> applyCont c v
                                            Nothing -> Right Undefined
interpHelp (Number x) c _ = applyCont c (Numb x)
interpHelp (Define _ _) _ _ = Right BadDefine

applyCont :: Cont -> Val -> Either Val InterpError
applyCont (BinL op y e k) x = interpHelp y (BinR op x k) e
applyCont (BinR op (Numb x) k) (Numb y) = applyCont k (Numb (opTrans op x y))
applyCont (AppL a e k) f = interpHelp a (AppR f k) e
applyCont (AppR (Closure fp fb e) k) x = interpHelp fb k ((fp,x):e)
applyCont MT x = Left x
applyCont _ _ = Right OtherInterpError
