module Eval where

import Prelude hiding (EQ,LT,GT)
import AbsMiniJS

-- NOTE that this is a VERY minimal version of evaluate.
-- Your real evaluate function must implement an environment,
-- mutable variables, checking of errors, and many more
-- formats of expressions. This is just a very small example
-- to get you started. 

-- Define you Value type here

{-
  Author: Xiaohui Chen
  EID: xc2388
-}

data Value = Integer Integer 
              | Bool Bool
              | Str String
              | NullV
              | Undefined
              | Closure [String] Seq Env
              | ItemV (String,Value)
              | RecordV [(String,Value)]
              | Address Int
              deriving (Eq, Show)

data BinaryOp = Add | Sub | Mul | Div | BiAnd | BiOr | GT | LT | LE | GE | EQ | NE
  deriving (Eq,Show)

data UnaryOp = Neg | Not
  deriving (Eq,Show)

type Env= [(String,Value)]
type Memory = [Value]
type Stateful t = Memory -> (Checked Value, Memory)

data Checked a = Good a
                 | Error String
          deriving Show

data JSComp t = JS (Memory->(Checked t,Memory))

instance Monad JSComp where
  return val = JS (\m -> (Good val, m))
  (JS c) >>= f = 
    JS (\m -> case c m of
              (Good val, m')-> let JS f' = f val
                               in f' m'
              (Error msg,m')->(Error msg,m'))
raise :: String->JSComp Value
raise msg=JS(\m->(Error msg,m))

runJSComp::JSComp a-> Checked a
runJSComp (JS f) = let (v,_)= f [] in v

runJSComp1::JSComp a->Memory->(Checked a,Memory)
runJSComp1 (JS f) mem = f mem


evaluate tree = case runJSComp (evaluateSeq tree []) of
                Good (Integer n)->show(n)
                Good (Bool True)-> "true"
                Good (Bool False)->"false"
                Good (Str str)->show(str)
                Good NullV->"null"
                Good Undefined->"undef"
                Good (RecordV _) ->"[object]"
                Error msg->"Error: "++msg
                _->"Syntax Error"

checkBinaryType :: BinaryOp->Value->Value->Bool
checkBinaryType BiOr (Bool a) (Bool b) = True
checkBinaryType BiAnd (Bool a) (Bool b) =True
checkBinaryType EQ (Bool a) (Bool b) =True
checkBinaryType EQ (Integer a) (Integer b) =True
checkBinaryType EQ (Str a) (Str b) = True
checkBinaryType EQ NullV  a = True
checkBinaryType EQ a NullV = True
checkBinaryType EQ Undefined a = True
checkBinaryType EQ a Undefined = True
checkBinaryType NE (Bool a) (Bool b) =True
checkBinaryType NE (Integer a) (Integer b) = True
checkBinaryType NE (Str a) (Str b) = True
checkBinaryType NE NullV  a = True
checkBinaryType NE a NullV = True
checkBinaryType NE Undefined a = True
checkBinaryType NE a Undefined = True
checkBinaryType LT (Integer a) (Integer b) =True
checkBinaryType LE (Integer a) (Integer b) =True
checkBinaryType GE (Integer a) (Integer b) =True
checkBinaryType GT (Integer a) (Integer b) =True
checkBinaryType Add (Integer a) (Integer b) =True
checkBinaryType Add (Str a) b = True
checkBinaryType Add b (Str a)  = True
checkBinaryType Sub (Integer a) (Integer b) =True
checkBinaryType Mul (Integer a) (Integer b) =True
checkBinaryType Div (Integer a) (Integer b) = True
checkBinaryType _ _ _ = False
                 
binaryOp :: BinaryOp->Value->Value->Value
binaryOp BiOr (Bool a) (Bool b) = Bool (a || b)
binaryOp BiAnd (Bool a) (Bool b) =Bool (a && b)
binaryOp EQ (Bool a) (Bool b) =Bool (a == b)
binaryOp EQ (Integer a) (Integer b) =Bool (a == b)
binaryOp EQ (Str a) (Str b) = Bool (a==b)
binaryOp EQ NullV  a = case a of
                       NullV->Bool True
                       _ -> Bool False
binaryOp EQ a NullV = case a of
                      NullV->Bool True
                      _ -> Bool False
binaryOp EQ Undefined a = case a of
                          Undefined->Bool True
                          _ -> Bool False
binaryOp EQ a Undefined = case a of
                          Undefined->Bool True
                          _ -> Bool False
binaryOp NE (Bool a) (Bool b) =Bool (a /= b)
binaryOp NE (Integer a) (Integer b) = Bool (a /= b)
binaryOp NE (Str a) (Str b) = Bool (a/=b)
binaryOp NE NullV  a = case a of
                       NullV->Bool False
                       _ -> Bool True
binaryOp NE a NullV = case a of
                      NullV->Bool False
                      _ -> Bool True
binaryOp NE Undefined a = case a of
                          Undefined->Bool False
                          _ -> Bool True
binaryOp NE a Undefined = case a of
                          Undefined->Bool False
                          _ -> Bool True
binaryOp LT (Integer a) (Integer b) =Bool (a < b)
binaryOp LE (Integer a) (Integer b) =Bool (a <= b)
binaryOp GE (Integer a) (Integer b) =Bool (a >= b)
binaryOp GT (Integer a) (Integer b) =Bool (a > b)
binaryOp Add (Integer a) (Integer b) =Integer (a + b)
binaryOp Add (Str a) b = case b of
                         Str str->Str (a++str)
                         NullV->Str (a++"null")
                         Undefined->Str (a++"undef")
                         Bool val->Str (a++(show val))
                         Integer val->Str (a++(show val))
                         RecordV val->Str (a++"[object]")
binaryOp Add b (Str a)  = case b of
                          Str str->Str (str++a)
                          NullV->Str ("null"++a)
                          Undefined->Str ("undef"++a)
                          Bool val->Str ((show val)++a)
                          Integer val->Str ((show val)++a)
                          RecordV val->Str ("[object]"++a)
binaryOp Sub (Integer a) (Integer b) =Integer (a - b)
binaryOp Mul (Integer a) (Integer b) =Integer (a * b)
binaryOp Div (Integer a) (Integer b) = Integer (a `div` b)


unaryOp :: UnaryOp->Value->Value
unaryOp Neg (Integer a) =Integer (-a)
unaryOp Not (Bool a) =Bool (not a)

checkUnaryType :: UnaryOp->Value->Bool
checkUnaryType Neg (Integer a) =True
checkUnaryType Not (Bool a) =True
checkUnaryType _ _ = False

evaluateIdent :: Ident -> String
evaluateIdent x = case x of
  Ident str  -> str


evaluateFuncSeq :: Seq->Env->JSComp Value
evaluateFuncSeq x env = case x of
  Var id exp seq  -> do
             av<-evaluateExp exp env 
             addr<-newMemory av
             evaluateFuncSeq seq ((evaluateIdent id, addr):env)  
               
               
  Sequence stat seq  -> do 
                     val<-evaluateFuncStat stat env
                     evaluateFuncSeq seq env
  Seqstat stat  -> evaluateFuncStat stat env


evaluateFuncStat :: Stat-> Env -> JSComp Value
evaluateFuncStat x  env= case x of
  BlockStat seq  -> evaluateFuncSeq seq env
  ExpStat exp  -> do
            av<-evaluateExp exp env
            return Undefined
  Return exp  -> evaluateExp exp env
  If exp stat0 stat  -> do
                       av<-evaluateExp exp env
                       case av of
                        (Bool v)-> evaluateFuncStat (if v then stat0 else stat) env
                        _-> raise ("Expected boolean but found " ++ show av)
  While exp stat  -> do
  av<-evaluateExp exp env
  case av of
   Bool True-> do 
          bv <- (evaluateFuncStat stat env)
          evaluateFuncStat (While exp stat) env
   Bool False-> return Undefined
   _-> raise ("Expected boolean but found " ++ show av)


evaluateSeq :: Seq->Env->JSComp Value
evaluateSeq x env = case x of
  Var id exp seq  -> do
             av<-evaluateExp exp env 
             addr<-newMemory av
             evaluateSeq seq ((evaluateIdent id, addr):env)  
               
               
  Sequence stat seq  -> do 
                        val<-evaluateStat stat env
                        evaluateSeq seq env
  Seqstat stat  -> evaluateStat stat env


evaluateStat :: Stat-> Env -> JSComp Value
evaluateStat x  env= case x of
  BlockStat seq  -> evaluateSeq seq env
  ExpStat exp  -> evaluateExp exp env
  Return exp  -> raise "Error is undefined";
  If exp stat0 stat  -> do
                       av<-evaluateExp exp env
                       case av of
                        (Bool v)-> evaluateStat (if v then stat0 else stat) env
                        _-> raise ("Expected boolean but found " ++ show av)
  While exp stat  -> do
  av<-evaluateExp exp env
  case av of
   Bool True-> do 
          bv <- (evaluateStat stat env)
          evaluateStat (While exp stat) env
   Bool False-> return Undefined
   _-> raise ("Expected boolean but found " ++ show av)
  
  


evaluateExp :: Exp -> Env-> JSComp Value
evaluateExp x env = case x of
  Assign exp0 exp  -> case exp0 of
                      Variable (Ident x)-> do
                               av<-evaluateExp exp env
                               tempVal<-evaluateExp exp0 env
                               addr<- getAddr x env
                               case addr of
                                 Address (-1)->raise "No such Variable"
                                 Address v-> updateMemory (Good av) v
                               
                               
  Or exp0 exp  -> do
         av<-evaluateExp exp0 env
         bv<-evaluateExp exp env
         if (checkBinaryType BiOr av bv)
          then return (binaryOp BiOr av bv)
          else raise("Type for binary operation is not correct")
  And exp0 exp  -> do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType BiAnd av bv
                  then return (binaryOp BiAnd av bv)
                  else raise("Type for binary operation is not correct")
  BinaryEQ exp0 exp  ->  do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType EQ av bv
                  then return (binaryOp EQ av bv)
                  else raise("Type for binary operation is not correct")
  BinaryNE exp0 exp  ->  do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType NE av bv
                  then return (binaryOp NE av bv)
                  else raise("Type for binary operation is not correct")
  BinaryLT exp0 exp  ->  do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType LT av bv
                  then return (binaryOp LT av bv)
                  else raise("Type for binary operation is not correct")
  BinaryLE exp0 exp  ->  do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType LE av bv
                  then return (binaryOp LE av bv)
                  else raise("Type for binary operation is not correct")
  BinaryGE exp0 exp  ->  do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType GE av bv
                  then return (binaryOp GE av bv)
                  else raise("Type for binary operation is not correct")
  BinaryGT exp0 exp  ->  do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType GT av bv
                  then return (binaryOp GT av bv)
                  else raise("Type for binary operation is not correct")
  BinaryAdd exp0 exp  ->  do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType Add av bv
                  then return (binaryOp Add av bv)
                  else raise("Type for binary operation is not correct")
  BinarySub exp0 exp  ->  do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType Sub av bv
                  then return (binaryOp Sub av bv)
                  else raise("Type for binary operation is not correct")
  BinaryMul exp0 exp  ->  do
                 av<-evaluateExp exp0 env
                 bv<-evaluateExp exp env
                 if checkBinaryType Mul av bv
                  then return (binaryOp Mul av bv)
                  else raise("Type for binary operation is not correct")
  BinaryDiv exp0 exp  ->  do
     av<-evaluateExp exp0 env
     bv<-evaluateExp exp env
     case bv of
      Integer 0 ->raise"divided by 0 error"
      _->if checkBinaryType Div av bv
         then return (binaryOp Div av bv)
         else raise("Type for binary operation is not correct")
  UnaryNot exp  -> do
                 av<-evaluateExp exp env
                 if checkUnaryType Not av
                  then return (unaryOp Not av)
                  else raise("Type for unary operation is not correct")
  UnaryNeg exp  -> do
                 av<-evaluateExp exp env
                 if checkUnaryType Neg av
                  then return (unaryOp Neg av)
                  else raise("Type for unary operation is not correct")
  Call exp exps  -> do
   closure<-evaluateExp exp env
   closure1<-mkNewClosure closure exps env
   case closure1 of
    Closure x seq env1-> case exp of
                          Field exp1 id -> do
                                 record<-evaluateExp exp1 env
                                 case record of
                                  RecordV rv-> evaluateFuncSeq seq (("&this",RecordV rv):(env1++env))
                                  _->evaluateFuncSeq seq (env1++env)
                          Lookup exp0 exp-> do
                                  record<-evaluateExp exp0 env
                                  case record of
                                   RecordV rv-> evaluateFuncSeq seq (("&this",RecordV rv):(env1++env))
                                   _->evaluateFuncSeq seq (env1++env)
                          _->evaluateFuncSeq seq (env1++env)
  Record exps  -> do
              newVal<-evaluateRecord exps env (RecordV [])
              temp<-newMemory newVal
              return newVal
  Item id exp  -> do
          av<-evaluateExp exp env
          return (ItemV (evaluateIdent id,av))
  Function ids seq  -> if noDup ids
                       then return (Closure args seq env)
                       else raise "Dupicate arguments"
                       where args=getArgus ids
  String str  -> return (Str str)
  Int n  -> return (Integer n)
  TrueLit  -> return (Bool True)
  FalseLit  -> return (Bool False)
  Null  -> return NullV
  Undef  -> return Undefined
  Variable id  -> case lookup (evaluateIdent id) env of
                  Nothing->return Undefined
                  Just (Address v)->readMemory v
                                     
  This  -> case env of
           ("&this",val):xs->return val
           _ ->return Undefined
  Field exp id  -> do
         record<-evaluateExp exp env
         case record of
          RecordV x->case lookup (evaluateIdent id) x of
                      Nothing->return Undefined
                      Just v-> return v
          _-> return Undefined
  Lookup exp0 exp  -> do
         record<-evaluateExp exp0 env
         case record of
          RecordV x-> do
                string<-evaluateExp exp env
                case string of
                 Str str-> case lookup str x of
                            Nothing->return Undefined
                            Just v-> return v
                 _->raise "The lookup syntax is not correct"
          _->return Undefined

evaluateRecord :: [Exp] -> Env-> Value-> JSComp Value
evaluateRecord [] env (RecordV x) = return (RecordV x)
evaluateRecord (y:ys) env (RecordV x) = do
          val<- evaluateExp y env
          case val of
           ItemV content-> evaluateRecord ys env (RecordV (x++[content]))
           _->raise "Item types are not correct"

newMemory val = JS (\mem-> (Good (Address (length mem)), mem ++ [val]))

readMemory i = JS (\mem-> (Good (access i mem), mem))

updateMemory (Good val) i = JS (\mem-> (Good val, update i val mem))

update addr val mem = 
 let (before, _:after) = splitAt addr mem in
    before ++ [val] ++ after

getAddr str env =JS( \mem-> case lookup str env of
                          Nothing ->(Good (Address (-1)),mem)
                          Just (Address v)->(Good (Address v),mem) )

access i mem=  if((i<=((length mem)-1)) && (i>=0)) 
                   then (mem !! i)
                   else Undefined

getArgus [] = []
getArgus (x:xs) = (evaluateIdent x):(getArgus xs)

noDup [] = True
noDup (x:xs) = if(x `elem` xs)
               then False
               else noDup xs

mkNewClosure :: Value->[Exp]->Env->JSComp Value
mkNewClosure (Closure x seq env1) exps env =JS( \mem->let (mem',env')= mkenv x exps env mem
                                                       in
                                                      if (length env')==0
                                                      then (Good (Closure x seq env1),mem') 
                                                      else case last env' of
                                                           ("&error",Str msg)-> (Error msg,mem)
                                                           _->(Good (Closure x seq (env'++env1)),mem'))
                                               

mkenv [] [] env mem= (mem,[])
mkenv (x:xs) (exp:exps) env mem = case runJSComp1 (evaluateExp exp env) mem of
                                 (Good val,mem')-> (as,b:bs)
                                              where
                                              as=mem'++[val]
                                              b=(x,Address ((length as)-1))
                                              (_,bs)=mkenv xs exps env as
                                 (Error msg,mem')->([],[("&error",Str msg)])

