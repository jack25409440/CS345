import Data.Maybe

data Op = Add | Sub | Mul | Div | And | Or | Not | Eq | Less | Great
    deriving (Eq, Show)
data Exp = Literal Value
           | Primitive Op [Exp]
           | Variable String
           | If Exp Exp Exp
           | Let [(String, Exp)] Exp
           | Fun [String] Exp
           | Call Exp [Exp]
           deriving (Show, Eq)
data Value = Number Int
            |Bool Bool
            | Closure [String] Exp Env
          deriving (Eq, Show)

data Checked a = Good a
                 | Error String
          deriving Show

type Env = [(String, Value)]

eval :: Env -> Exp -> Checked Value
eval e (Literal v) = Good v
eval e (Variable x) = case (lookup x e) of
                      Nothing -> Error ("Variable " ++ x ++ " undefined")
                      Just v  -> Good v
eval e (Primitive op l) = case eval e (l!!0) of
                          Error msg -> Error msg
                          Good av -> case eval e (l!!1) of
                                     Error msg -> Error msg
                                     Good bv -> (checked_binary op av bv)

eval e (If a b c) = case fromBool(eval e a) of
                    Error msg->Error msg
                    Good (Bool x)-> if x
                                    then case eval e b of
                                         Error msg->Error msg
                                         Good cked-> Good cked
                                    else case eval e c of
                                         Error msg->Error msg
                                         Good cked1->Good cked1

eval e (Let a b) = if noDup (fst(unzip a))
                   then  let newenv=[(n, eval (removeCheck newenv) b)| (n,b)<-a]
                         in case checkEnv newenv of
                            Error msg->Error msg
                            Good (Bool True)->case eval ((removeCheck newenv)++e) b of
                                              Error msg-> Error msg
                                              Good c1-> Good c1
                   else (Error "Duplicate elements in let!")

eval e (Fun arg body) =   if noDup arg
                          then Good (Closure arg body e)
                          else (Error "Duplicate function arguments!")
eval e (Call f args) =  case eval e f of
                        Error msg-> Error msg
                        Good c1-> let (Closure x exp env1)=c1
                                  in
                                  case mkcenv e x args of
                                  Error msg->Error msg
                                  Good c2->case eval (c2++env1) exp of
                                                Error msg->Error msg
                                                Good c3->Good c3

--helper functions
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
                   where tmpArray= [eval env ar|ar<-args]

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
fromBool (a) = Error "Type in the if-statement is not correct"



prim :: Op -> [Value] -> Value
prim Add [Number a, Number b] = Number (a+b)
prim Sub [Number a, Number b] = Number (a-b)
prim Mul [Number a, Number b] = Number (a*b)
prim Div [Number a, Number b] = Number (a `div` b)
prim And [Bool a, Bool b] = Bool (a && b)
prim Or [Bool a, Bool b] = Bool (a||b)
prim Not [Bool a] = Bool (not a)
prim Eq [Number a,Number b]= Bool (a==b)
prim Eq [Bool a, Bool b] = Bool ((a && b) || not(a || b))
prim Less [Number a, Number b] = Bool (a<b)
prim Great [Number a, Number b] = Bool (a>b)

checked_binary :: Op -> Value -> Value -> Checked Value
checked_binary Div (Number a) (Number b) =
  if b == 0
  then Error "Divide by zero"
  else Good (Number (a `div` b))
checked_binary op a b = Good (prim op [a,b])


f0=Fun ["x","y"] (Primitive Add [Variable "x",Variable "y"])
args0= [Variable "k",Literal (Number 2)]
f1=Fun ["f", "x"] (Call (Variable "f") [(Call (Variable "f") [Variable "x"])])
l0= Let [("f",f0)] (Call (Variable "f") [Literal (Number 3),Literal (Number 5)]) 
l1= Call (If (Literal (Bool True)) f0 f1) [Literal (Number 3),Literal(Number 5)]
fact=Fun ["n"] (If (Primitive Eq [Variable "n",Literal (Number 0)]) (Variable "one") (Primitive Mul [Variable "n",Call (Variable "fact") [Primitive Sub [Variable "n", Variable "one"]]]))
l2=Let [("one",Literal (Number 1)),("fact",fact)] (Call (Variable "fact") [Literal (Number 5)])
if1= If (Bool True) (Literal (Number 1)) (Literal (Number 0))
a= Primitive Div [Literal (Number 6),Literal (Number 0)]


main= do      
      print(eval [("k",Number 3)] (Variable "k"))
      print(eval [] a)
     print(eval [] (If (Literal (Bool True)) a (Literal (Number 5)) ))
      print(eval [("k",Number 3)] (Call f0 args0))
      print(eval [] if1)
      print(eval [] (Primitive Eq [Literal (Number 3),Literal (Number 3)]))

