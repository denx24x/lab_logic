{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Z]

tokens :-

  $white+                    ;
  "#".*                      ;
  \(                         { \_ -> LeftP }
  \)                         { \_ -> RightP }
  \|                         { \_ -> OrT }
  &                          { \_ -> AndT }
  "->"                       { \_ -> ImplT }
  !                          { \_ -> NotT }
  $alpha [$alpha $digit \39]*    { \s -> Ident s }

{

data Token = AndT
           | OrT
           | ImplT
           | NotT
           | LeftP
           | RightP
           | Ident String
           deriving (Show, Eq)

}
