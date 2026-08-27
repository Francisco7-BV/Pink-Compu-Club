{
module Grammars where

import Lexer (Token(..), lexer)
}

%name parse
%tokentype { Token }
%error { parseError }

%token
      nat             { TokenNum $$ }
      bool            { TokenBool $$ }
      '+'             { TokenSuma }
      '-'             { TokenResta }
      '*'             { TokenMul }
      '/'             { TokenDiv }
      "and"           { TokenAnd }
      "or"            { TokenOr }
      "not"           { TokenNot }
      "add1"          { TokenAdd1 }
      "sub1"          { TokenSub1 }
      "zero?"         { TokenZeroP }
      "expt"          { TokenExpt }
      '<'             { TokenLT }
      '>'             { TokenGT }
      "<="            { TokenLE }
      ">="            { TokenGE }
      "eq"            { TokenEq }
      '('             { TokenPA }
      ')'             { TokenPC }

%%

ASA : nat                      { Num $1 }
    | bool                     { Boolean $1 }
  
-- RETO 2:
-- Agrega las producciones para:
--   * operadores n-arios con al menos dos argumentos;

    | '(' '+' lista ')'        { Add $3 }
    | '(' '*' lista ')'        { Mul $3 }
    | '(' "and" lista ')'      { And $3 }
    | '(' "or" lista ')'       { Or $3 }

--   * operadores estrictamente binarios: expt y eq;

    | '(' '-' expr expr ')'    { Sub [$3, $4] }
    | '(' '/' expr expr ')'    { Div [$3, $4] }
    | '(' "expt" expr expr ')' { Expt $3 $4 }
    | '(' "eq" expr expr ')'   { EqP $3 $4 }
    | '(' '<' expr expr ')'    { Lt [$3, $4] }
    | '(' '>' expr expr ')'    { Gt [$3, $4] }
    | '(' "<=" expr expr ')'   { Le [$3, $4] }
    | '(' ">=" expr expr ')'   { Ge [$3, $4] }

--   * operadores unarios: not, add1, sub1, zero?.

    | '(' "not" expr ')'       { Not $3 }
    | '(' "add1" expr ')'      { Add1 $3 }
    | '(' "sub1" expr ')'      { Sub1 $3 }
    | '(' "zero?" expr ')'     { ZeroP $3 }

-- RETO 3:
-- Agrega un no terminal para representar dos o mas argumentos.
-- El resultado debe ser una lista de ASA.

{
parseError :: [Token] -> a
parseError toks = error ("Parse error: " ++ show toks)

data ASA
  = Num Int
  | Boolean Bool
  | And [ASA]
  | Or [ASA]
  | Add [ASA]
  | Sub [ASA]
  | Mul [ASA]
  | Div [ASA]
  | Lt [ASA]
  | Gt [ASA]
  | Le [ASA]
  | Ge [ASA]
  | Expt ASA ASA
  | EqP ASA ASA
  | Not ASA
  | Add1 ASA
  | Sub1 ASA
  | ZeroP ASA
  deriving (Eq, Show)
}
