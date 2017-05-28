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
interpNum x (e,s) = (interp x e s EmptyCont) >>= extract
 where extract (Numb y,s') = Right (y,(e,s'))
       extract _ = Left NotANumber

interpDefine :: String -> Ast -> State -> Either InterpError State
interpDefine x a (e,s) = (interp a e s EmptyCont) >>= addtostate
  where addtostate (v,s') = let l = newloc s' in Right ((x,l):e,(l,Found v):s')

newloc :: Store -> Loc
newloc = toInteger . length

data InterpCont = EmptyCont | BinL Op Ast Env InterpCont | BinR Op Integer InterpCont
                | AppL Ast Env InterpCont | Memoize Loc InterpCont

interp :: Ast -> Env -> Store -> InterpCont -> Either InterpError (Val,Store)
interp (Number v) _ s c = applyInterpCont c (Numb v) s
interp (Fun p b) e s c = applyInterpCont c (Closure p b e) s
interp (Bin op x y) e s c = interp x e s (BinL op y e c)
interp (App f x) e s c = interp f e s (AppL x e c)
interp (Var x) e s c = maybe (Left Undefined) takel (lookup x e)
  where takel l = maybe (Left BadLoc) (takemem l) (lookup l s)
        takemem l (Found y) = applyInterpCont c y s
        takemem l (Todo ast le) = interp ast le s (Memoize l c)
interp (Define _ _) _ _ _ = Left BadDefine

applyInterpCont :: InterpCont -> Val -> Store -> Either InterpError (Val,Store)
applyInterpCont EmptyCont x s = Right (x,s)
applyInterpCont (BinL op y e c) (Numb v) s = interp y e s (BinR op v c)
applyInterpCont (BinL _ _ _ _) _ _ = Left LeftOperandNotNumber
applyInterpCont (BinR op x c) (Numb y) s = applyInterpCont c (Numb (opTrans op x y)) s
applyInterpCont (BinR _ _ _) _ _ = Left RightOperandNotNumber
applyInterpCont (AppL x e c) (Closure fp fb fe) s
  = let l = newloc s in interp fb ((fp,l):fe) ((l,Todo x e):s) c
applyInterpCont (AppL _ _ _) _ _ = Left NotAFunction
applyInterpCont (Memoize l c) x s = applyInterpCont c x ((l,Found x):s)
