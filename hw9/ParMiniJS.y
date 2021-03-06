-- This Happy file was machine-generated by the BNF converter
{
module ParMiniJS where
import AbsMiniJS
import LexMiniJS
import ErrM
}

%name pSeq Seq
%name pStat Stat
%name pExp Exp
%name pExp1 Exp1
%name pExp2 Exp2
%name pExp3 Exp3
%name pExp4 Exp4
%name pExp5 Exp5
%name pExp6 Exp6
%name pExp7 Exp7
%name pExp8 Exp8
%name pListIdent ListIdent
%name pListExp ListExp

-- no lexer declaration
%monad { Err } { thenM } { returnM }
%tokentype { Token }

%token 
 '=' { PT _ (TS "=") }
 ';' { PT _ (TS ";") }
 '(' { PT _ (TS "(") }
 ')' { PT _ (TS ")") }
 '{' { PT _ (TS "{") }
 '}' { PT _ (TS "}") }
 '||' { PT _ (TS "||") }
 '&&' { PT _ (TS "&&") }
 '==' { PT _ (TS "==") }
 '!=' { PT _ (TS "!=") }
 '<' { PT _ (TS "<") }
 '<=' { PT _ (TS "<=") }
 '>=' { PT _ (TS ">=") }
 '>' { PT _ (TS ">") }
 '+' { PT _ (TS "+") }
 '-' { PT _ (TS "-") }
 '*' { PT _ (TS "*") }
 '/' { PT _ (TS "/") }
 '!' { PT _ (TS "!") }
 ':' { PT _ (TS ":") }
 '.' { PT _ (TS ".") }
 '[' { PT _ (TS "[") }
 ']' { PT _ (TS "]") }
 ',' { PT _ (TS ",") }
 'else' { PT _ (TS "else") }
 'false' { PT _ (TS "false") }
 'function' { PT _ (TS "function") }
 'if' { PT _ (TS "if") }
 'null' { PT _ (TS "null") }
 'return' { PT _ (TS "return") }
 'this' { PT _ (TS "this") }
 'true' { PT _ (TS "true") }
 'undefined' { PT _ (TS "undefined") }
 'var' { PT _ (TS "var") }
 'while' { PT _ (TS "while") }

L_ident  { PT _ (TV $$) }
L_quoted { PT _ (TL $$) }
L_integ  { PT _ (TI $$) }
L_err    { _ }


%%

Ident   :: { Ident }   : L_ident  { Ident $1 }
String  :: { String }  : L_quoted { $1 }
Integer :: { Integer } : L_integ  { (read $1) :: Integer }

Seq :: { Seq }
Seq : 'var' Ident '=' Exp ';' Seq { Var $2 $4 $6 } 
  | Stat Seq { Sequence $1 $2 }
  | Stat { Seqstat $1 }


Stat :: { Stat }
Stat : Exp ';' { ExpStat $1 } 
  | 'return' Exp ';' { Return $2 }
  | 'if' '(' Exp ')' Stat 'else' Stat { If $3 $5 $7 }
  | 'while' '(' Exp ')' Stat { While $3 $5 }
  | '{' Seq '}' { BlockStat $2 }


Exp :: { Exp }
Exp : Exp8 '=' Exp { Assign $1 $3 } 
  | Exp1 { $1 }


Exp1 :: { Exp }
Exp1 : Exp1 '||' Exp2 { Or $1 $3 } 
  | Exp2 { $1 }


Exp2 :: { Exp }
Exp2 : Exp2 '&&' Exp3 { And $1 $3 } 
  | Exp3 { $1 }


Exp3 :: { Exp }
Exp3 : Exp3 '==' Exp4 { BinaryEQ $1 $3 } 
  | Exp3 '!=' Exp4 { BinaryNE $1 $3 }
  | Exp4 { $1 }


Exp4 :: { Exp }
Exp4 : Exp4 '<' Exp5 { BinaryLT $1 $3 } 
  | Exp4 '<=' Exp5 { BinaryLE $1 $3 }
  | Exp4 '>=' Exp5 { BinaryGE $1 $3 }
  | Exp4 '>' Exp5 { BinaryGT $1 $3 }
  | Exp5 { $1 }


Exp5 :: { Exp }
Exp5 : Exp5 '+' Exp6 { BinaryAdd $1 $3 } 
  | Exp5 '-' Exp6 { BinarySub $1 $3 }
  | Exp6 { $1 }


Exp6 :: { Exp }
Exp6 : Exp6 '*' Exp7 { BinaryMul $1 $3 } 
  | Exp6 '/' Exp7 { BinaryDiv $1 $3 }
  | Exp7 { $1 }


Exp7 :: { Exp }
Exp7 : '!' Exp7 { UnaryNot $2 } 
  | '-' Exp7 { UnaryNeg $2 }
  | Exp7 '(' ListExp ')' { Call $1 $3 }
  | '{' ListExp '}' { Record $2 }
  | Ident ':' Exp7 { Item $1 $3 }
  | 'function' '(' ListIdent ')' '{' Seq '}' { Function $3 $6 }
  | String { String $1 }
  | Integer { Int $1 }
  | 'true' { TrueLit }
  | 'false' { FalseLit }
  | 'null' { Null }
  | 'undefined' { Undef }
  | Exp8 { $1 }


Exp8 :: { Exp }
Exp8 : Ident { Variable $1 } 
  | 'this' { This }
  | Exp8 '.' Ident { Field $1 $3 }
  | Exp8 '[' Exp ']' { Lookup $1 $3 }
  | '(' Exp ')' { $2 }


ListIdent :: { [Ident] }
ListIdent : {- empty -} { [] } 
  | Ident { (:[]) $1 }
  | Ident ',' ListIdent { (:) $1 $3 }


ListExp :: { [Exp] }
ListExp : {- empty -} { [] } 
  | Exp { (:[]) $1 }
  | Exp ',' ListExp { (:) $1 $3 }



{

returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ if null ts then [] else (" before " ++ unwords (map prToken (take 4 ts)))

myLexer = tokens
}

