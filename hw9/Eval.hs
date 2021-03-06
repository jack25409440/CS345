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

evaluate tree =  case evaluateSeq tree [] [] of
                  (Good (Bool True),mem')->"true"
                  (Good (Bool False),mem')->"false"
                  (Good (Str string),mem')->show(string)
                  (Good (Integer n),mem')->show(n) 
                  (Good Undefined ,mem')->"undef"
                  (Good NullV , mem')->"null"
                  (Good (RecordV val),mem')->"[object]"
                  (Error msg,mem')->"Error: "++msg 
                  {-case evaluateSeq tree [] [] of
                  (Good (Bool True),mem')->show((Bool True,mem'))
                  (Good (Bool False),mem')->show((Bool False,mem'))
                  (Good (Str string),mem')->show((Str string,mem'))
                  (Good (Integer n),mem')->show((Integer n,mem'))
                  (Good (RecordV v),mem')->show((RecordV v,mem')) 
                  (Good Undefined ,mem')->show((Undefined,mem'))
                  (Good NullV , mem')->show((NullV,mem'))
                  (Error msg,mem')->"Error: "++msg-}
                  


evaluateIdent :: Ident -> String
evaluateIdent x = case x of
  Ident str  -> str

evaluateFuncSeq x env mem = case x of
  Var id exp seq  -> {-let (newenv,mem'')= case evaluateExp exp env mem of
                                       (Good val,mem')->let mem1=mem'++[val]
                                                            addr = Address ((length mem'')-1)
                                                        in
                                                        ([(evaluateIdent id,addr)],mem1)
                                       (Error msg,mem')->([(msg,Address (-1))]++env,mem)
                     in
                     case checkEnv newenv of
                     Error msg->(Error msg,mem)
                     _->evaluateFuncSeq seq newenv mem'' -}
                     case evaluateExp exp env mem of
                     (Good val,mem') -> let mem''=mem'++[val]
                                            addr = Address ((length mem'')-1)
                                         in
                                         evaluateFuncSeq seq ((evaluateIdent id,addr):env) mem''
                     (Error msg,mem')->(Error msg,mem')       
  Sequence stat seq  -> case stat of
                        Return exp-> evaluateFuncStat stat env mem
                        _ -> case evaluateFuncStat stat env mem of
                             (Good val,mem') -> evaluateFuncSeq seq env mem'
                             (Error msg,mem')->(Error msg,mem')
  Seqstat stat  -> evaluateFuncStat stat env mem


evaluateFuncStat :: Stat -> Env-> Stateful Value
evaluateFuncStat x env mem = case x of
  ExpStat exp  -> case evaluateExp exp env mem of
                  (Good val,mem') -> (Good Undefined,mem')
                  (Error msg,mem')->(Error msg,mem')
  Return exp  -> evaluateExp exp env mem
  If exp stat0 stat -> case evaluateExp exp env mem of
                            (Good (Bool True),mem')-> evaluateFuncStat stat0 env mem'
                            (Good (Bool False),mem')->evaluateFuncStat stat env mem'
                            (Error msg,mem')->(Error msg,mem')
                            _ -> (Error "the condition is not of the right type", mem)
  While exp stat  -> case evaluateExp exp env mem of
                     (Good (Bool True),mem')->case evaluateFuncStat stat env mem' of
                                       (Good val,mem'')-> evaluateFuncStat (While exp stat) env mem''
                                       (Error msg,mem'')->(Error msg,mem'')
                     (Good (Bool False),mem')->(Good Undefined,mem')
                     (Error msg,mem')->(Error msg,mem')
                     _->(Error "the condition is not of the right type",mem)
  BlockStat seq  -> evaluateFuncSeq seq env mem

checkEnv [] = Good (Bool True)
checkEnv ((a,Address b):xs) = if b == (-1)
                                  then Error a
                                  else checkEnv xs

evaluateSeq :: Seq -> Env-> Stateful Value
evaluateSeq x env mem = case x of
  Var id exp seq  -> {-let (newenv,mem'')= case evaluateExp exp env mem of
                                       (Good val,mem')->let mem''=mem'++[val]
                                                            addr = Address ((length mem'')-1)
                                                        in
                                                        ([(evaluateIdent id,addr)]++env,mem'')
                                       (Error msg,mem')->([(msg,Address (-1))],mem)
                     in
                     case checkEnv newenv of
                     Error msg->(Error msg,mem)
                     _->evaluateSeq seq newenv mem''-}
                     case evaluateExp exp env mem of
                     (Good val,mem') -> let mem''=mem'++[val]
                                            addr = Address ((length mem'')-1)
                                         in
                                         evaluateSeq seq ((evaluateIdent id,addr):env) mem''
                     (Error msg,mem')->(Error msg,mem')
                     
  Sequence stat seq  -> case evaluateStat stat env mem of
                        (Good val,mem') -> evaluateSeq seq env mem'
                        (Error msg,mem')-> (Error msg,mem')
  Seqstat stat  -> evaluateStat stat env mem


evaluateStat :: Stat -> Env-> Stateful Value
evaluateStat x env mem = case x of
  ExpStat exp  -> evaluateExp exp env mem
  Return exp  -> (Error "return is not defined",mem)
  If exp stat0 stat -> case evaluateExp exp env mem of
                            (Good (Bool True),mem')-> evaluateStat stat0 env mem'
                            (Good (Bool False),mem')->evaluateStat stat env mem'
                            (Error msg,mem')->(Error msg,mem')
                            _ ->(Error "the condition is not of the right type", mem)
  While exp stat  -> case evaluateExp exp env mem of
                     (Good (Bool True),mem')->case evaluateStat stat env mem' of
                                       (Good val,mem'')-> evaluateStat (While exp stat) env mem''
                                       (Error msg,mem'')->(Error msg,mem'')
                     (Good (Bool False),mem')->(Good Undefined,mem')
                     (Error msg,mem')->(Error msg,mem')
                     _ ->(Error "the condition is not of the right type", mem)
  BlockStat seq  -> evaluateSeq seq env mem




binaryOp :: BinaryOp->Checked Value->Checked Value->Checked Value
binaryOp BiOr (Good (Bool a)) (Good (Bool b)) = Good( Bool (a || b))
binaryOp BiAnd (Good (Bool a)) (Good (Bool b)) =Good( Bool (a && b))
binaryOp EQ (Good (Bool a)) (Good (Bool b)) =Good( Bool (a == b))
binaryOp EQ (Good (Integer a)) (Good (Integer b)) =Good( Bool (a == b))
binaryOp EQ (Good (Str a)) (Good (Str b)) = Good (Bool (a==b))
binaryOp EQ (Good NullV) (Good NullV) = Good (Bool True)
binaryOp EQ (Good Undefined) (Good Undefined) = Good (Bool True)
binaryOp NE (Good (Bool a)) (Good (Bool b)) =Good( Bool (a /= b))
binaryOp NE (Good (Integer a)) (Good (Integer b)) =Good( Bool (a /= b))
binaryOp NE (Good (Str a)) (Good (Str b)) = Good (Bool (a/=b))
binaryOp NE (Good NullV) (Good a) = case a of
                                    NullV->Good (Bool False)
                                    _ -> Good (Bool True)
binaryOp NE (Good a) (Good NullV) = case a of
                                    NullV->Good (Bool False)
                                    _ -> Good (Bool True)
binaryOp NE (Good Undefined) (Good a) = case a of
                                        Undefined->Good (Bool False)
                                        _ -> Good (Bool True)
binaryOp NE (Good a) (Good Undefined) = case a of
                                        Undefined->Good (Bool False)
                                        _ -> Good (Bool True)
binaryOp LT (Good (Integer a)) (Good (Integer b)) =Good( Bool (a < b))
binaryOp LE (Good (Integer a)) (Good (Integer b)) =Good( Bool (a <= b))
binaryOp GE (Good (Integer a)) (Good (Integer b)) =Good( Bool (a >= b))
binaryOp GT (Good (Integer a)) (Good (Integer b)) =Good( Bool (a > b))
binaryOp Add (Good (Integer a)) (Good (Integer b)) =Good( Integer (a + b))
binaryOp Add (Good (Str a)) (Good b) = case b of
                                       Str str->Good (Str (a++str))
                                       NullV->Good (Str (a++"null"))
                                       Undefined->Good (Str (a++"undef"))
                                       Bool val->Good (Str (a++(show val)))
                                       Integer val->Good (Str (a++(show val)))
                                       RecordV val->Good (Str (a++"[object]"))
binaryOp Add (Good b) (Good (Str a))  = case b of
                                        Str str->Good (Str (str++a))
                                        NullV->Good (Str ("null"++a))
                                        Undefined->Good (Str ("undef"++a))
                                        Bool val->Good (Str ((show val)++a))
                                        Integer val->Good (Str ((show val)++a))
                                        RecordV val->Good (Str ("[object]"++a))
binaryOp Sub (Good (Integer a)) (Good (Integer b)) =Good( Integer (a - b))
binaryOp Mul (Good (Integer a)) (Good (Integer b)) =Good( Integer (a * b))
binaryOp Div (Good (Integer a)) (Good (Integer b)) =case b of
                                                    0->Error "divided by 0 error"
                                                    _->Good( Integer (a `div` b))
binaryOp _ _ _ =Error "no such binary operation"

unaryOp :: UnaryOp->Checked Value->Checked Value
unaryOp Neg (Good (Integer a)) =Good( Integer (-a))
unaryOp Not (Good (Bool a)) = Good (Bool (not a))
unaryOp _ _ = Error "no such unary operation"

update addr val mem = 
 let (before, _:after) = splitAt addr mem in
    before ++ [val] ++ after

access i mem = if((i<=((length mem)-1)) && (i>=0)) 
               then (mem !! i)
               else Undefined

getAddr str env = case lookup str env of
                  Nothing -> -1
                  Just (Address v)-> v                   
                       

evaluateExp :: Exp ->Env-> Stateful Value
evaluateExp x env mem = case x of
  Assign exp0 exp  -> case exp0 of
                      Variable (Ident x)-> case evaluateExp exp env mem of
                                             (Good val,mem')-> case evaluateExp exp0 env mem' of
                                                              (Good val1,mem'')->  case getAddr x env of
                                                                                   -1->(Error "No such variable",mem'')
                                                                                   v->(Good val,update v val mem'')
                                                              (Error msg,mem'')->(Error msg,mem'')
                                             (Error msg,mem')->(Error msg,mem')
  Or exp0 exp  -> case evaluateExp exp0 env mem of
                  (Error msg,mem')->(Error msg,mem')
                  (av,mem')-> case evaluateExp exp env mem' of
                              (Error msg,mem'')->(Error msg,mem'')
                              (bv,mem'')-> (binaryOp BiOr av bv, mem'')
  And exp0 exp  ->case evaluateExp exp0 env mem of
	          (Error msg,mem')->(Error msg,mem')
                  (av,mem')-> case evaluateExp exp env mem' of
                              (Error msg,mem'')->(Error msg,mem'')
                              (bv,mem'')-> (binaryOp BiAnd av bv, mem'')
  BinaryEQ exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
                                  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp EQ av bv, mem'')
  BinaryNE exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
				  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp NE av bv, mem'')
  BinaryLT exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
  				  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp LT av bv, mem'')
  BinaryLE exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
				  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp LE av bv, mem'')
  BinaryGE exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
				  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp GE av bv, mem'')
  BinaryGT exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
				  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp GT av bv, mem'')
  BinaryAdd exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
				  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp Add av bv, mem'')
  BinarySub exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
				  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp Sub av bv, mem'')
  BinaryMul exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
				  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp Mul av bv, mem'')
  BinaryDiv exp0 exp  ->case evaluateExp exp0 env mem of
                      (Error msg,mem')->(Error msg,mem')
                      (av,mem')-> case evaluateExp exp env mem' of
				  (Error msg,mem'')->(Error msg,mem'')
                                  (bv,mem'')-> (binaryOp Div av bv, mem'')
  UnaryNot exp  -> case evaluateExp exp env mem of
                   (Error msg,mem')->(Error msg,mem')
                   (av,mem') -> (unaryOp Not av,mem')
  UnaryNeg exp  ->case evaluateExp exp env mem of
                   (Error msg,mem')->(Error msg,mem')
                   (av,mem') -> (unaryOp Neg av,mem')
  Call exp exps  -> case evaluateExp exp env mem of
                    (Good (Closure x seq env1),mem')-> case mkenv x exps env mem' of
                                                       (mem'',envs)-> case last envs of
                                                                      ("&error",Str msg)->(Error msg,[])
                                                                      _->case evaluateFuncSeq seq ((decideObject exp env mem)++(envs++env1)) mem'' of
                                                                         (Error msg,mem''')->(Error msg,mem''')
                                                                         (vals,mem'')->(vals,mem'')
                     
                    (Error msg,mem')->(Error msg,mem')
  Record exps  -> case groupEval exps env mem of
                  (vals,mem')-> case last vals of
                                Error msg->(Error msg,mem')
                                _->(Good (RecordV (removeItemV vals)),mem')
  Item id exp  -> case evaluateExp exp env mem of
                  (Error msg,mem')->(Error msg,mem')
                  (Good val,mem')->(Good (ItemV (evaluateIdent id,val)),mem')
  Function ids seq  -> if noDup ids
                       then (Good (Closure args seq env),mem)
                       else (Error "Dupicate arguments",mem)
                       where args=getArgus ids
  String str  -> (Good (Str str),mem)
  Int n  -> (Good (Integer n),mem)
  TrueLit  -> (Good (Bool True),mem)
  FalseLit  -> (Good (Bool False),mem)
  Null  -> (Good NullV ,mem)
  Undef  -> (Good Undefined,mem)
  Variable id  -> case lookup (evaluateIdent id) env of
                  Nothing -> (Good Undefined, mem)
                  Just (Address v)-> case access v mem of
                                     Undefined->(Error "No such address",mem)
                                     val->(Good val,mem)
  This  -> case env of
           ("&this",val):xs ->(Good val,mem)
           _->(Good Undefined,mem)
  Field exp id  -> case evaluateExp exp env mem of
                   (Good (RecordV x),mem')-> case lookup (evaluateIdent id) x of
                                             Nothing -> (Good Undefined,mem')        --(Error "No such variable in the record",mem')
                                             Just v -> (Good v,mem')  
                   (Error msg,mem')->(Error msg,mem')
                   (_,mem')->(Good Undefined,mem')
  Lookup exp0 exp  -> case evaluateExp exp0 env mem of
                      (Good (RecordV x),mem')-> case evaluateExp exp env mem' of
                                                (Good (Str string),mem'')-> case lookup string x of
                                                                            Nothing -> (Good Undefined,mem') --error "No such variable in the record"
                                                                            Just v -> (Good v,mem'')
                                                _->(Error "the variable syntax is not valid",mem')
                      (Error msg,mem')->(Error msg,mem')
                      (_,mem')->(Good Undefined,mem')
  --_->(Error "Syntax Error",mem)



decideObject x env mem = case x of
                         Field exp id-> case evaluateExp exp env mem of
                                        (Good (RecordV x),mem')->[("&this",RecordV x)]
                                        _->[]
                         Lookup exp0 exp->case evaluateExp exp0 env mem of
                                          (Good (RecordV x),mem')->[("&this",RecordV x)]
                                          _->[]
                         _->[]

removeItemV [] = []
removeItemV (x:xs)=case x of
                   Good (ItemV v)-> v:(removeItemV xs)
                   _ -> error "Item error"

groupEval [] env mem = ([],mem)
groupEval (e:exps) env mem = case evaluateExp e env mem of
                             (Good val,mem')->(Good val:a,b)
                                         where
                                         (a,b)= groupEval exps env mem'
                             (Error msg,mem')->([Error msg],[])


mkenv [] [] env mem = (mem,[])
mkenv (x:xs) (exp:exps) env mem = case evaluateExp exp env mem of
                                 (Good val,mem')-> (as,b:bs)
                                              where
                                              as=mem'++[val]
                                              b=(x,Address ((length as)-1))
                                              (_,bs)=mkenv xs exps env as
                                 (Error msg,mem')->([],[("&error",Str msg)])

getArgus [] = []
getArgus (x:xs) = (evaluateIdent x):(getArgus xs)

noDup [] = True
noDup (x:xs) = if(x `elem` xs)
               then False
               else noDup xs


