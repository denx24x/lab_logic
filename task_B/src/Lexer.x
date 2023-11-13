{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [A-Z]

tokens :-
  '\r'                        ;
  '\t'                        ;
  \n                        {\_ -> NextLine}
  $white+                    ;
  "#".*                      ;
  "|-"                        {\_ -> Proove}
  \(                         { \_ -> LeftP }
  \)                         { \_ -> RightP }
  \|                         { \_ -> OrT }
  &                          { \_ -> AndT }
  "->"                       { \_ -> ImplT }
  !                          { \_ -> NotT }
  ","                        {\_ -> Sep}
  $alpha [$alpha $digit \39]*    { \s -> Ident s }
{

data Token = AndT
           | OrT
           | ImplT
           | NotT
           | LeftP
           | RightP
           | Proove
           | NextLine
           | Ident String
           | Sep
           deriving (Show, Eq)

}
