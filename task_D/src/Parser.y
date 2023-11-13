{
module Parser where

import Grammar
import Lexer
}

%name      parseExpr
%tokentype { Token }
%error     { parseError }
%monad     { Either String }{ >>= }{ return }

%token IDENT  { Ident $$ }
%token NOT    { NotT }
%token AND    { AndT }
%token OR     { OrT }
%token IMPL   { ImplT }
%token LEFTP  { LeftP }
%token RIGHTP { RightP }
%token NEXTLINE { NextLine }
%token PROOVE { Proove }
%token SEP    { Sep }
%token FALSE { FalseVal }


%right IMPL
%left OR 
%left AND 
%nonassoc NOT

%%

Expr
  : IDENT                   { Var $1 }
  | FALSE                { FVal }
  | Expr AND Expr           {Binary And $1 $3 }
  | Expr OR Expr            {Binary Or $1 $3 }
  | Expr IMPL Expr          {Binary Impl $1 $3 }
  | LEFTP Expr RIGHTP       {$2}

{
parseError = fail "Parse error"
}
