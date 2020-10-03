module Eval1
  ( eval
  , State
  )
where

import           AST
import qualified Data.Map.Strict               as M
import           Data.Strict.Tuple

-- Estados
type State = M.Map Variable Int

-- Estado nulo
initState :: State
initState = M.empty

-- Busca el valor de una variable en un estado
lookfor :: Variable -> State -> Int
lookfor v s = s M.! v

-- Cambia el valor de una variable en un estado
update :: Variable -> Int -> State -> State
update = M.insert

-- Evalua un programa en el estado nulo
eval :: Comm -> State
eval p = stepCommStar p initState

-- Evalua multiples pasos de un comnado en un estado,
-- hasta alcanzar un Skip
stepCommStar :: Comm -> State -> State
stepCommStar Skip s = s
stepCommStar c    s = Data.Strict.Tuple.uncurry stepCommStar $ stepComm c s

-- Evalua un paso de un comando en un estado dado
stepComm :: Comm -> State -> Pair Comm State
stepComm Skip s = (Skip :!: s)
stepComm (Let x e) s = (Skip :!: s'')
                       where
                         (n :!: s') = evalExp e s
                         s'' = update x n s'
stepComm (Seq c0 c1) s = stepComm c1 s' where (_ :!: s') = stepComm c0 s 
stepComm (IfThenElse e c0 c1) s = if b 
                                    then stepComm c0 s'
                                    else stepComm c1 s'
                                  where (b :!: s') = evalExp e s
stepComm w@(While e c) s = if b
                            then stepComm (Seq c w) s'
                            else (Skip :!: s')
                           where (b :!: s') = evalExp e s

-- Evalua una expresion
evalExp :: Exp a -> State -> Pair a State
-- Expresiones enteras
evalExp (Const n) s = (n :!: s)
evalExp (Var x) s = (n :!: s)
                    where n = lookfor x s
evalExp (UMinus e) s = ((-n) :!: s')
                       where (n :!: s') = evalExp e s
evalExp (Plus e e') s = (n :!: s'')
                         where
                           (n0 :!: s') = evalExp e s
                           (n1 :!: s'') = evalExp e' s'
                           n = n0  + n1
evalExp (Minus e e') s = (n :!: s'')
                         where
                           (n0 :!: s') = evalExp e s
                           (n1 :!: s'') = evalExp e' s'
                           n = n0 - n1
evalExp (Times e e') s = (n :!: s'')
                          where
                           (n0 :!: s') = evalExp e s
                           (n1 :!: s'') = evalExp e' s'
                           n = n0 * n1
evalExp (Div e e') s = (n :!: s'')
                         where
                           (n0 :!: s') = evalExp e s
                           (n1 :!: s'') = evalExp e' s'
                           n = div n0 n1
evalExp (EAssgn x e) s = (n :!: s'')
                          where 
                            (n :!: s') = evalExp e s
                            s'' = update x n s
evalExp (ESeq e e') s = evalExp e' s' where (_ :!: s') = evalExp e s
-- Expresiones booleanas
evalExp BTrue s = (True :!: s)
evalExp BFalse s = (False :!: s)
evalExp (Not e) s = (not b :!: s')
                    where (b :!: s') = evalExp e s
evalExp (Lt e e') s = (n0 < n1 :!: s'')
                       where 
                         (n0 :!: s') = evalExp e s
                         (n1 :!: s'') = evalExp e' s'
evalExp (Gt e e') s = (n0 > n1 :!: s'')
                       where 
                         (n0 :!: s') = evalExp e s
                         (n1 :!: s'') = evalExp e' s'
evalExp (Eq e e') s = (n0 == n1 :!: s'')
                       where 
                        (n0 :!: s') = evalExp e s
                        (n1 :!: s'') = evalExp e' s'
evalExp (NEq e e') s = (n0 /= n1 :!: s'')
                        where 
                          (n0 :!: s') = evalExp e s
                          (n1 :!: s'') = evalExp e' s'
evalExp (And e e') s = (b0 && b1 :!: s'')
                        where 
                          (b0 :!: s') = evalExp e s
                          (b1 :!: s'') = evalExp e' s'                                                       
evalExp (Or e e') s = ((b0 || b1) :!: s'')
                        where 
                          (b0 :!: s') = evalExp e s
                          (b1 :!: s'') = evalExp e' s'