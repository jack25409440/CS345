import Data.Maybe


data Op = Add | Sub | Mul | Div | And | Or | Not | Eq | Less | Great
    deriving (Eq, Show)
data Exp = Literal Value
           | Primitive Op [Exp]
           | Variable String
           | If Exp Exp Exp
           | Let [(String, Exp)] Exp
           deriving (Show, Eq)
data Value = Number Int
            |Bool Bool
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
                   then (eval ([(x,eval e d)|(x,d)<-a]++e) b)
                   else (error "Duplicate elements!")
--helper functions
noDup [] = True
noDup (x:xs) = if (x `elem` xs)
               then False
               else noDup xs

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

main= do
      --test prim function
      putStr "prim Add [Number 3, Number 2] = "
      print (prim Add [Number 3, Number 2])
      putStr "prim Sub [Number 3, Number 2] = "
      print (prim Sub [Number 3, Number 2])
      putStr "prim Mul [Number 3, Number 2] = "
      print (prim Mul [Number 3, Number 2])
      putStr "prim Div [Number 3, Number 2] = "
      print (prim Div [Number 3, Number 2])
      putStr "prim And [Bool True,Bool True] = "
      print (prim And [Bool True,Bool True])
      putStr "prim And [Bool True,Bool False] = "
      print (prim And [Bool True,Bool False])
      putStr "prim And [Bool False,Bool False] = "
      print (prim And [Bool False,Bool False])
      putStr "prim Or [Bool True,Bool True] = "
      print (prim Or [Bool True,Bool True])
      putStr "prim Or [Bool True,Bool False] = "
      print (prim Or [Bool True,Bool False])
      putStr "prim Or [Bool False,Bool False] = "
      print (prim Or [Bool False,Bool False])
      putStr "prim Not [Bool True] = "
      print (prim Not [Bool True])
      putStr "prim Not [Bool False] = "
      print (prim Not [Bool False])
      putStr "prim Eq [Number 3,Number 3] = "
      print (prim Eq [Number 3,Number 3])
      putStr "prim Eq [Number 3,Number 5] = "
      print (prim Eq [Number 3,Number 5])
      putStr "prim Eq [Bool False, Bool False] = "
      print (prim Eq [Bool False, Bool False])
      putStr "prim Eq [Bool True, Bool True] = "
      print (prim Eq [Bool True, Bool True])
      putStr "prim Eq [Bool True, Bool False] = "
      print (prim Eq [Bool True, Bool False])
      putStr "prim Less [Number 3, Number 5] = "
      print (prim Less [Number 3, Number 5])
      putStr "prim Less [Number 5, Number 3] = "
      print (prim Less [Number 5, Number 3])
      putStr "prim Great [Number 3, Number 5] = "
      print (prim Great [Number 3, Number 5])
      putStr "prim Great [Number 5, Number 3] = "
      print (prim Great [Number 5, Number 3])

      --test eval function
      putStr "eval 5+3 : "
      print (eval [] (Primitive Add [Literal (Number 3), Literal (Number 5)]))
      putStr "eval 5+3 with variable x : "
      print (eval [("x",Number 3)] (Primitive Add [Variable "x", Literal (Number 5)]))
      putStr "eval 2+3+5 : "
      print (eval [] (Primitive Add [Literal (Number 2),(Primitive Add [Literal (Number 3), Literal (Number 5)]) ] ))
      putStrLn "eval If statement : "
      print (eval [] (If (Primitive Eq [Literal (Number 3),Literal (Number 5)]) (Literal (Number 10)) (Literal (Number 20)) ))
      print (eval [("x", Number 5)] (If (Primitive Eq [Variable "x",Literal (Number 5)]) (Literal (Number 10)) (Literal (Number 20)) ))
      print (eval [("x", Number 5)] (If (Primitive Eq [Variable "x",Literal (Number 5)]) (If (Primitive Eq [Literal (Bool False), Literal (Bool False)]) (Literal (Number 5)) (Literal (Number 10))) (Literal (Number 20)) ))
      --exception
      --print (eval [("x", Number 5)] (If (Literal (Number 5)) (Literal (Number 10)) (Literal (Number 20)) ))
      putStrLn "eval Let Statement"
      print (eval [] (Let [("x",Literal (Number 3))] (Primitive Add [Variable "x", Literal (Number 2)])))
      print (eval [] (Let [("x",Literal (Number 3))] (Let [("x",Literal (Number 2))] (Primitive Add [Variable "x", Literal (Number 2)])) ))
      print (eval [] (Let [("x",Literal (Number 3)), ("y", Literal (Number 2))] (Primitive Add [Variable "x", Variable "y"])))
      --exception
      --print (eval [] (Let [("x",Literal (Number 3)), ("x", Literal (Number 2))] (Primitive Add [Variable "x", Literal (Number 2)])))
