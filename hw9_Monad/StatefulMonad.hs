module StatefulMonad where

import Prelude hiding (LT, GT, EQ, id)
import Base
import Data.Maybe
import MutableState hiding (Stateful, evaluate)

data Stateful t = ST (Memory -> (t, Memory))

instance Monad Stateful where
  return val = ST (\m -> (val, m))
  (ST c) >>= f = 
    ST (\m -> 
      let (val, m') = c m
          ST f' = f val
      in f' m')
        
evaluate :: Exp -> Env -> Stateful Value
evaluate exp env = eval exp where
    eval (Literal v) = return v
    eval (Unary op a) = do
      av <- eval a
      return (unary op av)
    eval (Binary op a b) = do
      av <- eval a
      bv <- eval b
      return (binary op av bv)
    eval (If a b c) = do
      BoolV cond <- eval a
      eval (if cond then b else c)
    eval (Let x e body) = do    -- non-recursive case
      ev <- eval e
      let newEnv = (x, ev) : env
      evaluate body newEnv
    eval (Variable x) = return (fromJust (lookup x env))
    eval (Function x body) = return (ClosureV  x body env)
    eval (Call fun arg) = do
      ClosureV  x body closeEnv <- eval fun
      argv <- eval arg
      let newEnv = (x, argv) : closeEnv
      evaluate body newEnv
    eval (Mutable e) = do
      ev <- eval e
      newMemory ev        
    eval (Access a) = do
      AddressV i <- eval a
      readMemory i
    eval (Assign a e) = do
      AddressV i <- eval a
      ev <- eval e
      updateMemory ev i
      return ev

newMemory val = ST (\mem-> (AddressV (length mem), mem ++ [val]))

readMemory i = ST (\mem-> (access i mem, mem))

updateMemory val i = ST (\mem-> ((), update i val mem))

runStateful (ST c) = 
   let (val, mem) = c [] in val

t0 = Let "x" (Literal (IntV 99)) (Variable "x")
t1 = Let "x" (Mutable (Literal (IntV 3)))
         (Access (Variable "x"))
         
main = do
  print (runStateful (evaluate (Literal (IntV 6)) []))
  print (runStateful (evaluate t0 []))
  print (runStateful (evaluate t1 []))
  
