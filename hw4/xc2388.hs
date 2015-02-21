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

type Env = [(String, Value)]

eval :: Env -> Exp -> Value
eval e (Literal v) = v
eval e (Variable x) = fromJust (lookup x e)
eval e (Primitive op l) = prim op [eval e p|p<-l]
eval e (If a b c) = if fromBool(eval e a)
                    then eval e b
                    else eval e c
eval e (Let a b) = if noDup (fst(unzip a))
                   then  eval newenv b     
                   else (error "Duplicate elements!")
                   where newenv=[(n,eval newenv b)|(n,b)<-a]++e
eval e (Fun arg body) =   if noDup arg
                          then Closure arg body e
                          else (error "Duplicate elements!")
eval e (Call f args) = eval env exp
                    where (Closure x exp env1) = eval e f
                          env = [(st,eval e ar)|(st,ar)<-(zip x args)]++env1
--helper functions
noDup [] = True
noDup (x:xs) = if (x `elem` xs)
               then False
               else noDup xs

fix a= a (fix a)

fromBool (Bool x) = x
fromBool (a) = error "Type is not correct"

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

f0=Fun ["x","y"] (Primitive Add [Variable "x",Variable "y"])
f1=Fun ["f", "x"] (Call (Variable "f") [(Call (Variable "f") [Variable "x"])])
l0= Let [("f",f0)] (Call (Variable "f") [Literal (Number 3),Literal (Number 5)]) 
l1= Call (If (Literal (Bool True)) f0 f1) [Literal (Number 3),Literal(Number 5)]
fact=Fun ["n"] (If (Primitive Eq [Variable "n",Literal (Number 0)]) (Variable "one") (Primitive Mul [Variable "n",Call (Variable "fact") [Primitive Sub [Variable "n", Variable "one"]]]))
l2=Let [("one",Literal (Number 1)),("fact",fact)] (Call (Variable "fact") [Literal (Number 10)])

main= do      
      print(eval [] (Let [("f",Fun ["x"] (Primitive Mul [Variable "x",Literal (Number 2)]) )] (Call (Variable "f") [(Literal (Number 3))]))    )
      print(eval [] (Fun ["x"] (Fun ["y"] (Primitive Add [Variable "x",Variable "y"]))))
      --exception
      --print(eval [] (Fun ["x","x"] (Primitive Add [Variable "x",Variable "x"])))
      print(eval [] (Call (Fun ["x"] (Primitive Add [Variable "x",Literal (Number 3)])) [Literal (Number 2)]))
      print(eval [("x",Number 3)] f1)
      print(eval [] l0)
      print(eval [] l1)
      print(eval [] l2)
     
