import Prelude hiding (EQ,LT,GT)
import Data.Maybe

data BinaryOp = Add | Sub | Mul | Div | And | Or | GT | LT | LE | GE | EQ
  deriving (Eq,Show)

data UnaryOp = Neg | Not
  deriving (Eq,Show)

data Exp = Literal Value
           | Unary UnaryOp Exp
           | Binary BinaryOp Exp Exp
           | Variable String
           | If Exp Exp Exp
           | Let [(String, Exp)] Exp --enable multiple bindings
           | Fun [String] Exp --multiple arguments
           | Call Exp [Exp] --multiple calling arguments
           | Try Exp Exp
           | Mutable   Exp        
           | Access    Exp         
           | Assign    Exp Exp 
           deriving (Show, Eq)
data Value = Int Int
            |Bool Bool
            | Closure [String] Exp Env
            |Address Int
          deriving (Eq, Show)

data Checked a = Good a
                 | Error String
          deriving Show

type Env = [(String, Value)]
type Memory = [Value]
type Stateful t = Memory -> (Checked Value, Memory)

access i mem = if((i<=((length mem)-1)) && (i>=0)) 
               then (Good (mem !! i))
               else Error "Access Error. No such Address"

update :: Int -> Value -> Memory -> Memory
update addr val mem =
  let (before, _:after) = splitAt addr mem in
    before ++ [val] ++ after

eval :: Exp -> Env -> Stateful Value
eval (Literal v) e mem = (Good v, mem)
eval (Variable x) e mem = case (lookup x e) of
                          Nothing -> (Error ("Variable " ++ x ++ " undefined"), mem)
                          Just v  -> (Good v,mem)
eval (Binary op a b) e mem = case eval a e mem of
                             (Error msg, mem')-> (Error msg,  mem')
                             (Good av, mem')-> case eval b e mem' of
                                               (Error msg, mem'')-> (Error msg, mem'')
                                               (Good bv, mem'') ->((checked_binary op av bv), mem'')
eval (Unary op a) e mem= case eval a e mem of
                         (Error msg,mem')->(Error msg, mem')
                         (Good av, mem')->((checked_unary op av),mem')
eval (If a b c) e mem = case fromBool(fst(eval a e mem)) of
                        Error msg->(Error msg, mem)
                        Good (Bool x)-> if x
                                            then case eval b e mem of
                                                 (Error msg,mem'')->(Error msg,mem'')
                                                 (Good cked,mem'')-> (Good cked, mem'')
                                            else case eval c e mem of
                                                 (Error msg, mem'')->(Error msg, mem'')
                                                 (Good cked1, mem'')->(Good cked1, mem'')
eval (Let a b) e mem = if noDup (fst(unzip a))
                       then  let  
                                newenv=zip names (fst(unzip(groupEval exps (removeCheck newenv) mem)))
                                curMem=last(snd(unzip(groupEval exps (removeCheck newenv) mem)))
                             in 
                             case checkEnv(newenv) of
                             Error msg1->(Error msg1,mem)
                             Good (Bool True)-> case eval b (removeCheck(newenv)++e) curMem of
                                                (Error msg,mem')-> (Error msg,mem')
                                                (Good c1,mem')-> (Good c1,mem')
                        else (Error "Duplicate elements in let!",mem)
                        where 
                        names=fst(unzip a)
                        exps=snd(unzip a)


eval (Fun arg body) e mem =   if noDup arg
                              then (Good (Closure arg body e),mem)
                              else (Error "Duplicate function arguments!",mem)
eval (Call f args) e mem =  case eval f e mem of
                            (Error msg,mem')-> (Error msg,mem)
                            (Good c1,mem')-> let (Closure x exp env1)=c1
                                  in
                                  if((length x)==(length args))
                                  then case mkcenv e x args mem' of
                                       (Error msg, mem'')->(Error msg,mem'')
                                       (Good c2,mem'')->case eval exp (c2++env1) mem'' of
                                                        (Error msg,mem''')->(Error msg,mem''')
                                                        (Good c3,mem''')->(Good c3,mem''')
                                  else (Error "Wrong number of arguments",mem')
eval (Try exp1 exp2) e mem = case eval exp1 e mem of
                             (Good v, mem')-> (Good v, mem')
                             (Error msg, mem)-> eval exp2 e mem
eval (Mutable exp) e mem = case eval exp e mem of
                           (Good val, mem')->  (Good (Address (length mem')), mem' ++ [val])
                           (Error msg, mem')-> (Error msg, mem')                        
eval (Access addr) e mem = case eval addr e mem of
                        (Good (Address i),mem')-> (access i mem', mem')
                        (Error msg,mem')-> (Error msg,mem')
eval (Assign addr exp) e mem = case eval addr e mem of
                              (Good (Address i),mem')-> case eval exp e mem' of
                                                 (Good ev, mem'') -> (Good ev,update i ev mem'')
                                                 (Error msg,mem'') -> (Error msg,mem'')
                              (Error msg,mem')->(Error msg,mem')
                              _ -> (Error "The first argument is not an address", mem)

--helper functions

getName (Variable x)=x

checkTypes :: BinaryOp->Value->Value->Bool
checkTypes Add x y = case x of
                     Int xv-> case y of
                                 Int yv-> True
                                 _ -> False
                     _-> False

checkTypes Sub x y = case x of
                     Int xv-> case y of
                                 Int yv-> True
                                 _ -> False
                     _ -> False

checkTypes Mul x y = case x of
                     Int xv-> case y of
                                 Int yv-> True
                                 _ -> False
                     _ -> False

checkTypes Div x y = case x of
                     Int xv-> case y of
                                 Int yv-> True
                                 _ -> False
                     _ -> False
checkTypes GT x y = case x of
                     Int xv-> case y of
                                 Int yv-> True
                                 _ -> False
                     _ -> False
checkTypes GE x y = case x of
                     Int xv-> case y of
                                 Int yv-> True
                                 _ -> False
                     _ -> False

checkTypes And x y = case x of
                     Bool xv-> case y of
                               Bool yv -> True
                               _-> False
                     _ -> False

checkTypes Or x y = case x of
                     Bool xv-> case y of
                               Bool yv -> True
                               _-> False
                     _ -> False
checkTypes LT x y = case x of
                     Int xv-> case y of
                                 Int yv-> True
                                 _ -> False
                     _ -> False
checkTypes LE x y = case x of
                     Int xv-> case y of
                                 Int yv-> True
                                 _ -> False
                     _ -> False
checkTypes EQ x y= case x of
                   Bool xv1-> case y of
                              Bool yv1->True
                              _-> False
                   Int xv2-> case y of
                                Int yv2->True
                                _->False
                   _->False

checkUTypes :: UnaryOp->Value->Bool
checkUTypes Neg x= case x of
                   Int xv->True
                   _->False
checkUTypes Not x= case x of
                   Bool xv->True
                   _->False


removeCheck [] = []
removeCheck ((a,b):xs)  = case b of
                        Error msg-> removeCheck xs
                        Good v-> (a,v):(removeCheck xs)

checkEnv [] = Good (Bool True)
checkEnv ((a,b):xs) = case b of
                      Error msg-> Error msg
                      Good v-> (checkEnv xs)
mkcenv env x args mem= if(checkValid tmpArray1)
                       then (Good ((zip x (removeGood tmpArray1))++env), last(snd(unzip tmpArray)))
                       else (getError tmpArray1, mem)
                       where 
                         tmpArray= (groupEval args env mem) 
                         tmpArray1=fst(unzip tmpArray)

groupEval [] env mem = []
groupEval (a:ar) env mem = result:(groupEval ar env (snd result))
                           where result= (eval a env mem)

evalEnvs [] env mem=[]
evalEnvs (x:xs) env mem = evals:(evalEnvs xs env mem')
                          where
                          evals=eval x env mem
                          mem'= snd(evals)

removeGood [] = []
removeGood ((Good x):xs)=x:(removeGood xs)

checkValid []=True
checkValid ((Good x):xs)=checkValid xs
checkValid ((Error x):xs)=False

getError [] = Error "No errors at all, please check the program"
getError ((Good x):xs) = getError xs
getError ((Error x):xs) = Error x

noDup [] = True
noDup (x:xs) = if (x `elem` xs)
               then False
               else noDup xs


fromBool (Good (Bool x)) =Good (Bool x)
fromBool (Error msg)= Error msg
fromBool (a) = Error "Type in the if-statement is not correct"



prim :: BinaryOp -> [Value] -> Value
prim Add [Int a, Int b] = Int (a+b)
prim Sub [Int a, Int b] = Int (a-b)
prim Mul [Int a, Int b] = Int (a*b)
prim Div [Int a, Int b] = Int (a `div` b)
prim And [Bool a, Bool b] = Bool (a && b)
prim Or [Bool a, Bool b] = Bool (a||b)
prim EQ [Int a,Int b]= Bool (a==b)
prim EQ [Bool a, Bool b] = Bool ((a && b) || not(a || b))
prim LT [Int a, Int b] = Bool (a<b)
prim LE [Int a, Int b] = Bool (a<=b)
prim GT [Int a, Int b] = Bool (a>b)
prim GE [Int a, Int b] = Bool (a>=b)

unary :: UnaryOp->Value->Value
unary Not (Bool b) = Bool (not b)
unary Neg (Int i)  = Int (-i)

checked_binary :: BinaryOp -> Value -> Value -> Checked Value
checked_binary Div (Int a) (Int b) =
  if b == 0
  then Error "Divide by zero"
  else Good (Int (a `div` b))
checked_binary op a b = if (checkTypes op a b)
                        then Good (prim op [a,b])
                        else (Error "Operand types are not correct")

checked_unary :: UnaryOp -> Value -> Checked Value
checked_unary op a= if (checkUTypes op a)
                    then Good (unary op a)
                    else (Error "Operand type is not correct")

f0=Fun ["x","y"] (Binary Add (Variable "x") (Variable "y"))
args0= [Variable "k",Literal (Int 2)]
f1=Fun ["f", "x"] (Call (Variable "f") [(Call (Variable "f") [Variable "x"])])
l0= Let [("f",f0)] (Call (Variable "f") [Literal (Int 3),Literal (Int 5)]) 
l1= Call (If (Literal (Bool True)) f0 f1) [Literal (Int 3),Literal(Int 5)]
fact=Fun ["n"] (If (Binary EQ (Variable "n") (Literal (Int 0))) (Variable "one") (Binary Mul (Variable "n") (Call (Variable "fact") [Binary Sub (Variable "n") (Variable "one") ])))
l2=Let [("one",Literal (Int 1)),("fact",fact)] (Call (Variable "fact") [Literal (Int 5)])
a= Binary Div (Literal (Int 6)) (Literal (Int 0))
l3= Let [("b", Binary Add (Literal (Bool True)) (Literal (Int 3)))] (Variable "b")
l4= Let [("a",a)] (Literal (Int 3))
l5= Let [("a",a)] (Variable "a")
l6= Let [("a", Access (Literal (Address 1)))] (Binary Add (Variable "a") (Literal (Int 2)))
l7= Let [("a",Mutable (Literal (Int 4)))] (Access (Literal (Address 0)))
l8= Let [("a",Mutable (Literal (Int 4)))] (Assign (Literal (Address 0)) (Literal (Int 100)))
try1= Try l1 (Literal (Int 100))
try2= Try l3 l4



main= do      
     print(eval (Mutable l4) [] [Bool True])
     print(eval l6 [] [Int 0])
     print(eval l2 [] [])
     print(eval l7 [] [Bool True])
     print(eval l8 [] [Bool True, Bool False])
