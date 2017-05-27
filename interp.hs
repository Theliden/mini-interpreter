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

interpNum :: Ast -> State -> Either InterpError (Integer,State)
interpNum x (e,s) = (interp x e s) >>= extract
 where extract (Numb y,s') = Right (y,(e,s'))
       extract _ = Left NotANumber

interpDefine :: String -> Ast -> State -> Either InterpError State
interpDefine x a (e,s) = (interp a e s) >>= addtostate
  where addtostate (v,s') = let l = newloc s' in Right ((x,l):e,(l,Found v):s')

newloc :: Store -> Loc
newloc = toInteger . length

interp :: Ast -> Env -> Store -> Either InterpError (Val,Store)
interp (Number v) _ s = Right (Numb v,s)
interp (Fun p b) e s = Right (Closure p b e,s)
interp (Bin op x y) e s = (interp x e s) >>= takeleft
  where takeleft (Numb v,s') = (interp y e s') >>= takeright v
        takeleft _ = Left LeftOperandNotNumber
        takeright lv (Numb w,s'') = Right (Numb (opTrans op lv w),s'')
        takeright _  _ = Left RightOperandNotNumber
interp (App f x) e s = (interp f e s) >>= takefun
  where takefun (Closure fp fb fe,s')
          = let l = newloc s' in interp fb ((fp,l):fe) ((l,Todo x e):s')
        takefun _ =  Left NotAFunction
interp (Var x) e s = maybe (Left Undefined) takel (lookup x e)
  where takel l = maybe (Left BadLoc) (takemem l) (lookup l s)
        takemem l (Found y) = Right (y,s)
        takemem l (Todo ast le) = (interp ast le s) >>= memoize l
        memoize l (z,s') = Right (z,(l,Found z):s')
interp (Define _ _) _ _ = Left BadDefine
