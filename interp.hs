-- interp.hs
-- Robert Cummings
-- May 2017

-- Interp:
-- Interprets an AST to a value.
-- Uses lazy evaluation with memoization.

module Interp(interpNum,interpDefine,State,emptyState) where

import Parsing

data Val = Numb Integer
         | Closure String Ast Env

type Loc = Integer
type Env = [(String, Loc)]

data MemCell = Todo Ast Env | Found Val
type Store = [(Loc,MemCell)]

type State = (Env,Store)

emptyState :: State
emptyState = ([],[])

opTrans :: Op -> Integer -> Integer -> Integer
opTrans Plus = (+)
opTrans Times = (*)

data InterpError = NotANumber | BadDefine | Undefined
                 | LeftOperandNotNumber | RightOperandNotNumber
                 | NotAFunction | OtherInterpError | BadLoc deriving Show

interpNum :: Ast -> State -> Either (Integer,State) InterpError
interpNum x (e,s) = case interp x e s of
                    Left (Numb y,s') -> Left (y,(e,s'))
                    Left _ -> Right NotANumber
                    Right er -> Right er

interpDefine :: String -> Ast -> State -> Either State InterpError
interpDefine x a (e,s)
  = case interp a e s of
      Right er -> Right er
      Left (v,s') -> Left (addToState x v (e,s'))

newloc :: Store -> Loc
newloc = toInteger . length

addToState :: String -> Val -> State -> State
addToState x v (e,s) = let l = newloc s in ((x,l):e,(l,Found v):s)

interp :: Ast -> Env -> Store -> Either (Val,Store) InterpError
interp (Number v) _ s = Left (Numb v,s)
interp (Fun p b) e s = Left (Closure p b e,s)
interp (Bin op x y) e s
  = case interp x e s of
      Right er -> Right er
      Left (Numb v,s')
        -> case interp y e s' of
             Right er -> Right er
             Left (Numb w,s'') -> Left (Numb (opTrans op v w),s'')
             _ -> Right RightOperandNotNumber
      _ -> Right LeftOperandNotNumber
interp (App f x) e s
  = case interp f e s of
      Right er -> Right er
      Left (Closure fp fb fe,s')
        -> let l = newloc s' in
             interp fb ((fp,l):fe) ((l,Todo x e):s')
      _ -> Right NotAFunction
interp (Var x) e s
  = case lookup x e of
      Just l
        -> case (lookup l s) of
             Just (Found y) -> Left (y,s)
             Just (Todo y le)
               -> case interp y le s of
                    Right er -> Right er
                    Left (z,s') -> Left (z,(l,Found z):s')
             Nothing -> Right BadLoc
      Nothing -> Right Undefined
interp (Define _ _) _ _ = Right BadDefine
