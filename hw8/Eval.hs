module Eval where

import Prelude hiding (EQ,LT,GT)
import AbsMiniJS

-- NOTE that this is a VERY minimal version of evaluate.
-- Your real evaluate function must implement an environment,
-- mutable variables, checking of errors, and many more
-- formats of expressions. This is just a very small example
-- to get you started. 

-- Define you Value type here

data Value = Integer Integer 
              | Bool Bool
              | Str String
              | Closure [String] Seq Env
              | RecordV [(String,Value)]
              | List [Value]
              deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Div | BiAnd | BiOr | GT | LT | LE | GE | EQ | NE
  deriving (Eq,Show)

data UnaryOp = Neg | Not
  deriving (Eq,Show)

type Env= [(String,Integer)]

binaryOp :: BinaryOp->Value->Value->Value
binaryOp BiOr (Bool a) (Bool b) = Bool (a || b)
binaryOp BiAnd (Bool a) (Bool b) = Bool (a && b)
binaryOp EQ (Bool a) (Bool b) = Bool (a == b)
binaryOp EQ (Integer a) (Integer b) = Bool (a == b)
binaryOp EQ (Str a) (Str b) = Bool (a==b)
binaryOp NE (Bool a) (Bool b) = Bool (a /= b)
binaryOp NE (Integer a) (Integer b) = Bool (a /= b)
binaryOp NE (Str a) (Str b) = Bool (a /= b)
binaryOp LT (Integer a) (Integer b) = Bool (a < b)
binaryOp LE (Integer a) (Integer b) = Bool (a <= b)
binaryOp GE (Integer a) (Integer b) = Bool (a >= b)
binaryOp GT (Integer a) (Integer b) = Bool (a > b)
binaryOp Add (Integer a) (Integer b) = Integer (a + b)
binaryOp Add (Str a) (Str b) = Str (a++b)
binaryOp Sub (Integer a) (Integer b) = Integer (a - b)
binaryOp Mul (Integer a) (Integer b) = Integer (a * b)
binaryOp Div (Integer a) (Integer b) = Integer (a `div` b)

unaryOp :: UnaryOp->Value->Value
unaryOp Neg (Integer a) = Integer (-a)
unaryOp Not (Bool a) = Bool (not a)



evaluate :: Seq->String
evaluate tree = case evaluateSeq tree [] of
                Integer a->show(a)
                Bool True->"true"
                Bool False->"false"
                Str str->show(str)

evaluateSeq (Seqstat stat) env = evaluateStat stat env
evaluateSeq (Var id exp seq) env = case id of
                                   Ident str->evaluateSeq seq ((str,evaluateExp exp env):env)
evaluateSeq (Sequence stat seq) env =  case evaluateStat stat env of
                                       val-> evaluateSeq seq env


evaluateStat (ExpStat exp) env = evaluateExp exp env
evaluateStat (BlockStat seq) env = evaluateSeq seq env

evaluateExp (Or exp0 exp) env = binaryOp BiOr (evaluateExp exp0 env) (evaluateExp exp env)
evaluateExp (And exp0 exp) env = binaryOp BiAnd (evaluateExp exp0 env) (evaluateExp exp env)
evaluateExp (BinaryEQ exp0 exp) env = binaryOp EQ (evaluateExp exp0 env) (evaluateExp exp env)
evaluateExp (BinaryNE exp0 exp) env = binaryOp NE (evaluateExp exp0 env) (evaluateExp exp env)
evaluateExp (BinaryLT exp0 exp) env = binaryOp LT (evaluateExp exp0 env) (evaluateExp exp env)
evaluateExp (BinaryLE exp0 exp) env = binaryOp LE (evaluateExp exp0 env) (evaluateExp exp env)
evaluateExp (BinaryGE exp0 exp) env = binaryOp GE (evaluateExp exp0 env) (evaluateExp exp env)
evaluateExp (BinaryGT exp0 exp) env = binaryOp GT (evaluateExp exp0 env) (evaluateExp exp env)
evaluateExp (BinaryAdd a b) env = binaryOp Add (evaluateExp a env) (evaluateExp b env)
evaluateExp (BinarySub a b) env = binaryOp Sub (evaluateExp a env) (evaluateExp b env)
evaluateExp (BinaryMul a b) env = binaryOp Mul (evaluateExp a env) (evaluateExp b env)
evaluateExp (BinaryDiv a b) env = binaryOp Div (evaluateExp a env) (evaluateExp b env)
evaluateExp (UnaryNot exp) env = unaryOp Not (evaluateExp exp env)
evaluateExp (UnaryNeg exp) env = unaryOp Neg (evaluateExp exp env)
evaluateExp (String str) env = Str str
evaluateExp (TrueLit) env = Bool True
evaluateExp (FalseLit) env = Bool False
evaluateExp (Int n) env= Integer n
evaluateExp (Variable id) env = case id of
                            Ident str->case lookup str env of
                                       Nothing-> error "Variable not defined"
                                       Just v-> v 
