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
           deriving (Show, Eq)
data Value = Int Int
            |Bool Bool
            | Closure [String] Exp Env
          deriving (Eq, Show)

data Checked a = Good a
                 | Error String
          deriving Show

type Env = [(String, Value)]

eval :: Exp -> Env -> Checked Value
eval (Literal v) e = Good v
eval (Variable x) e = case (lookup x e) of
                      Nothing -> Error ("Variable " ++ x ++ " undefined")
                      Just v  -> Good v
eval (Binary op a b) e = case eval a e of
                        Error msg-> Error msg
                        Good av-> case eval b e of
                                  Error msg-> Error msg
                                  Good bv->(checked_binary op av bv)
eval (Unary op a) e= case eval a e of
                     Error msg->Error msg
                     Good av->(checked_unary op av)
eval (If a b c) e = case fromBool(eval a e) of
                    Error msg->Error msg
                    Good (Bool x)-> if x
                                    then case eval b e of
                                         Error msg->Error msg
                                         Good cked-> Good cked
                                    else case eval c e of
                                         Error msg->Error msg
                                         Good cked1->Good cked1

eval (Let a b) e = if noDup (fst(unzip a))
                   then  let newenv=[(n,eval b (removeCheck newenv))| (n,b)<-a]
                          in 
                           case eval b (removeCheck(newenv)++e) of
                                               Error msg-> if((getName b) `elem` (fst(unzip newenv)))
                                                           then case checkEnv(newenv) of
                                                                Error msg1->Error msg1
                                                                Good (Bool True)-> Error msg
                                                           else Error msg
                                               Good c1-> Good c1
                   else (Error "Duplicate elements in let!")


eval (Fun arg body) e =   if noDup arg
                          then Good (Closure arg body e)
                          else (Error "Duplicate function arguments!")
eval (Call f args) e =  case eval f e of
                        Error msg-> Error msg
                        Good c1-> let (Closure x exp env1)=c1
                                  in
                                  if((length x)==(length args))
                                  then case mkcenv e x args of
                                       Error msg->Error msg
                                       Good c2->case eval exp (c2++env1) of
                                                Error msg->Error msg
                                                Good c3->Good c3
                                  else Error "Wrong number of arguments"
eval (Try exp1 exp2) e = case eval exp1 e of
                         Good v-> Good v
                         Error msg-> eval exp2 e

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
mkcenv env x args= if(checkValid tmpArray)
                   then Good ((zip x (removeGood tmpArray))++env)
                   else getError tmpArray
                   where tmpArray= [eval ar env|ar<-args]

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
try1= Try l1 (Literal (Int 100))
try2= Try l3 l4

main= do      
      print(eval (Unary Neg (Literal (Int (-5)))) [])
      print(eval (Unary Not (Literal (Bool False))) [])
      print(eval (Unary Neg (Literal (Bool False))) [])
      print(eval (Unary Not (Literal (Int (-5)))) [])
      print(eval (Binary Add (Literal (Int 3)) (Literal (Int 5))) [])
      print(eval l2 [])
      print(eval l3 [])
      print(eval try1 [])
      print(eval try2 [])
