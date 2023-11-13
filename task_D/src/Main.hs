module Main where

import Grammar
import Lexer (alexScanTokens)
import Parser (parseExpr)
import Data.List
import Data.Map
import Data.Set
import Data.Maybe
import System.IO(isEOF)

t_ax :: [Expr] -> Expr -> Tree
t_ax gamma phi = (Tree (ExprLine gamma phi "Ax") [])

t_e_arrow :: Tree -> Tree-> Tree
t_e_arrow left @ (Tree (ExprLine context1 (Binary Impl a1 b1) _) _) right @ (Tree (ExprLine context2 b2 _) _) | a1 == b2 = (Tree (ExprLine context1 b1 "E->") [left, right])
t_e_arrow right @ (Tree (ExprLine context2 b2 _) _) left @ (Tree (ExprLine context1 (Binary Impl a1 b1) _) _)  | a1 == b2 = (Tree (ExprLine context1 b1 "E->") [left, right])


t_i_arrow :: Tree -> Tree
t_i_arrow cur @ (Tree (ExprLine context b _) root) = (Tree (ExprLine (tail context) (Binary Impl (head context) b) "I->") [cur]) where 

t_i_and :: Tree -> Tree -> Tree
t_i_and first @ (Tree (ExprLine context1 a _) _) second @ (Tree (ExprLine context2 b _) _ ) = (Tree (ExprLine context1 (Binary And a b) "I&") [first, second])

t_el_and :: Tree -> Tree
t_el_and cur @ (Tree (ExprLine context (Binary And a b) _) _) = (Tree (ExprLine context a "El&") [cur])

t_er_and :: Tree -> Tree
t_er_and cur @ (Tree (ExprLine context (Binary And a b) _) _) = (Tree (ExprLine context b "Er&") [cur])

t_il_or :: Tree -> Expr -> Tree
t_il_or cur @ (Tree (ExprLine context a _) _) expr = (Tree (ExprLine context (Binary Or a expr) "Il|") [cur])

t_ir_or :: Tree -> Expr -> Tree
t_ir_or cur @ (Tree (ExprLine context a _) _) expr = (Tree (ExprLine context (Binary Or expr a) "Ir|") [cur])

t_e_or :: Tree -> Tree -> Tree -> Tree
t_e_or first @ (Tree (ExprLine context1 expr1 _) _) second @ (Tree (ExprLine context2 expr2 _) _) third @ (Tree (ExprLine context3 (Binary Or or1 or2) _) _) = (Tree (ExprLine context3 expr1 "E|") [first, second, third])
  where a = (head context1)
        b = (head context2)

t_e_not :: Tree -> Tree
t_e_not cur @ (Tree (ExprLine context FVal _) _)  = (Tree (ExprLine (tail context) a "E!!") [cur])
  where (Binary Impl a FVal) = (head context)

collectVars :: Expr -> Data.Set.Set String -> Data.Set.Set String
collectVars (Var name) vals = (Data.Set.insert name vals)
collectVars (Binary op a b) vals = (collectVars b (collectVars a vals))
collectVars other vals = vals

collectWrapper :: Expr -> [String]
collectWrapper expr = (Data.Set.toList (collectVars expr Data.Set.empty))

generateValues :: [String] -> [Data.Map.Map String Bool] -> Data.Map.Map String Bool -> [Data.Map.Map String Bool]
generateValues [] vals now = (now : vals)
generateValues (cur:names) vals now  = ((generateValues names vals (Data.Map.insert cur True now)) ++ (generateValues names vals (Data.Map.insert cur False now)))

proofValues :: Expr -> [String] -> Data.Map.Map String Bool -> Tree
proofValues expr [] now = proof now expr
proofValues expr (cur:names) now  = (joinRes left right) 
  where left = (proofValues expr names (Data.Map.insert cur True now))
        right = (proofValues expr names (Data.Map.insert cur False now))
  
findFalse :: Expr -> [Data.Map.Map String Bool] -> String
findFalse expr (cur:other) | not (calc expr cur) = (intercalate "," (Prelude.map (\(a, b) -> a ++ ":=" ++ (if b then "T" else "F")) (Data.Map.toList cur)))
findFalse expr (cur:other) = findFalse expr other

mshow :: Tree -> Int -> IO ()

data ExprLine = ExprLine [Expr] Expr String 
data Tree = Tree ExprLine [Tree]

subshow :: [Tree] -> Int -> IO ()
subshow [] n = return ()
subshow (cur : other) n = do 
  (mshow cur n)
  subshow other n

mshow (Tree (ExprLine context expr reason) other) n = do
    subshow other (n + 1)
    putStrLn ("[" ++ (show n) ++ "] " ++ ((intercalate "," (Data.List.map show context))  ++ "|-" ++ (show expr))  ++ " [" ++ (reason)++ "]")
    return ()

ax1 :: [Expr] -> Expr -> Expr -> Tree
ax1 context a b = (t_i_arrow 
                    (t_i_arrow 
                      (t_ax ([b, a] ++ context) a)
                    )
                  )

ax2 :: [Expr] -> Expr -> Expr -> Expr -> Tree
ax2 context a b c = (t_i_arrow
                      (t_i_arrow
                        (t_i_arrow
                          (t_e_arrow
                            (t_e_arrow
                              (t_ax ([a, (Binary Impl a (Binary Impl b c)), (Binary Impl a b)] ++ context) (Binary Impl a (Binary Impl b c)))
                              (t_ax ([a, (Binary Impl a (Binary Impl b c)), (Binary Impl a b)] ++ context) a)
                            )
                            (t_e_arrow
                              (t_ax ([a, (Binary Impl a (Binary Impl b c)), (Binary Impl a b)] ++ context) (Binary Impl a b))
                              (t_ax ([a, (Binary Impl a (Binary Impl b c)), (Binary Impl a b)] ++ context) a)
                            )
                          )
                        )
                      )
                    )

ax3 :: [Expr] -> Expr -> Expr -> Tree
ax3 context a b = (t_i_arrow 
                    (t_i_arrow 
                      (t_i_and 
                        (t_ax [b, a] a) 
                        (t_ax [b, a] b)
                      )
                    )
                  )

ax4 :: [Expr] -> Expr -> Expr -> Tree
ax4 context a b = (t_i_arrow 
                    (t_el_and 
                      (t_ax ([(Binary And a b)] ++ context) (Binary And a b))
                    )
                  )

ax5 :: [Expr] -> Expr -> Expr -> Tree
ax5 context a b = (t_i_arrow 
                    (t_er_and 
                      (t_ax ([(Binary And a b)] ++ context) (Binary And a b))
                    )
                  )

ax6 :: [Expr] -> Expr -> Expr -> Tree
ax6 context a b = (t_i_arrow
                    (t_il_or
                      (t_ax ([a] ++ context) a)
                      b
                    )
                  )

ax8 :: [Expr] -> Expr -> Expr -> Expr -> Tree
ax8 context a b p = (t_i_arrow (t_i_arrow (t_i_arrow (t_e_or 
                      (t_e_arrow 
                        (t_ax ([a, (Binary Or a b), (Binary Impl b p), (Binary Impl a p)] ++ context) (Binary Impl a p))
                        (t_ax ([a, (Binary Or a b), (Binary Impl b p), (Binary Impl a p)] ++ context) a)
                      )
                      (t_e_arrow 
                        (t_ax ([b, (Binary Or a b), (Binary Impl b p), (Binary Impl a p)] ++ context) (Binary Impl b p))
                        (t_ax ([b, (Binary Or a b), (Binary Impl b p), (Binary Impl a p)] ++ context) b)
                      )
                      (t_ax ([(Binary Or a b), (Binary Impl b p), (Binary Impl a p)] ++ context) (Binary Or a b))
                    ))))

ax9 :: [Expr] -> Expr -> Expr -> Tree
ax9 context a b = (t_i_arrow 
                    (t_i_arrow
                      (t_i_arrow
                        (t_e_arrow
                          (t_e_arrow
                            ((t_ax ([a, (Binary Impl a (Binary Impl b FVal)), (Binary Impl a b)] ++ context) (Binary Impl a b)))
                            ((t_ax ([a, (Binary Impl a (Binary Impl b FVal)), (Binary Impl a b)] ++ context) a))
                          )
                          (t_e_arrow
                            ((t_ax ([a, (Binary Impl a (Binary Impl b FVal)), (Binary Impl a b)] ++  context) (Binary Impl a (Binary Impl b FVal))))
                            ((t_ax ([a, (Binary Impl a (Binary Impl b FVal)), (Binary Impl a b)] ++ context) a))
                          )
                        )
                      )
                    )
                  )

ax10 :: [Expr] -> Expr -> Tree
ax10 context a = (t_i_arrow
                    (t_e_not
                      (t_e_arrow
                        (t_ax ([(Binary Impl a FVal), (Binary Impl (Binary Impl a FVal) FVal)] ++ context) (Binary Impl (Binary Impl a FVal) FVal))
                        (t_ax ([(Binary Impl a FVal), (Binary Impl (Binary Impl a FVal) FVal)] ++  context) (Binary Impl a FVal))
                      )
                    )
                  )

proof :: Data.Map.Map String Bool -> Expr -> Tree
proof eval FVal = (t_i_arrow (t_ax (([FVal] ++ getContext eval)) FVal))
proof eval cur @ (Var name) | (calc cur eval) = (t_ax (getContext eval) cur)
proof eval cur @ (Var name) = (t_ax (getContext eval) (inv cur))
proof eval cur @ (Binary Impl v @ (Var name) FVal) | (calc cur eval) = (t_ax (getContext eval) cur)
--proof eval cur @ (Binary Impl v @ (Var name) FVal)  = (t_ax (getContext eval) cur)

proof eval (Binary Impl a b) = (proofImpl eval a b)
proof eval (Binary Or a b) = (proofOr eval a b )
proof eval (Binary And a b) = (proofAnd eval a b)


checkNegative ::  Data.Map.Map String Bool -> Expr -> Expr
checkNegative eval expr | (calc expr eval) = expr
checkNegative eval expr = (Binary Impl expr FVal)

inv :: Expr -> Expr
inv v = (Binary Impl v FVal)

proofOr :: Data.Map.Map String Bool -> Expr -> Expr -> Tree
proofOr eval a b | (calc a eval) && (calc b eval) = (t_il_or (proof eval a) b)
proofOr eval a b | (not (calc a eval)) && (calc b eval) = (t_ir_or (proof eval b) a)
proofOr eval a b | (calc a eval) && (not (calc b eval)) = (t_il_or (proof eval a) b)
proofOr eval a b | (not (calc a eval)) && (not (calc b eval)) = (t_e_arrow
                                                                  (t_e_arrow
                                                                    (t_i_arrow
                                                                      (t_i_arrow
                                                                        (t_i_arrow
                                                                          (t_e_or
                                                                            (t_e_arrow
                                                                              (t_ax ([a, (Binary Or a b), (inv b), (inv a)] ++ (getContext eval)) (inv a))
                                                                              (t_ax ([a, (Binary Or a b), (inv b), (inv a)] ++ (getContext eval)) a)
                                                                            )
                                                                            (t_e_arrow
                                                                              (t_ax ([b, (Binary Or a b), (inv b), (inv a)] ++ (getContext eval)) (inv b))
                                                                              (t_ax ([b, (Binary Or a b), (inv b), (inv a)] ++ (getContext eval)) b)
                                                                            )
                                                                            (t_ax ([(Binary Or a b), (inv b), (inv a)] ++ (getContext eval)) (Binary Or a b))
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                    (proof eval a)
                                                                  )
                                                                  (proof eval b)
                                                                )
proofAnd :: Data.Map.Map String Bool -> Expr -> Expr -> Tree
proofAnd eval a b | (calc a eval) && (calc b eval) = (t_i_and (proof eval a) (proof eval b))
proofAnd eval a b | (calc a eval) && (not (calc b eval)) = (t_e_arrow
                                                              (t_i_arrow
                                                                (t_i_arrow
                                                                  (t_e_arrow
                                                                    (t_ax ([(Binary And a b), (inv b)] ++ (getContext eval)) (inv b))
                                                                    (t_er_and 
                                                                      (t_ax ([(Binary And a b), (inv b)] ++ (getContext eval)) (Binary And a b))
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                              (proof eval b)
                                                            )
proofAnd eval a b | (not (calc a eval)) && (calc b eval) = (t_e_arrow
                                                              (t_i_arrow
                                                                (t_i_arrow
                                                                  (t_e_arrow
                                                                    (t_ax ([(Binary And a b), (inv a)] ++ (getContext eval)) (inv a))
                                                                    (t_el_and 
                                                                      (t_ax ([(Binary And a b), (inv a)] ++ (getContext eval)) (Binary And a b))
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                              (proof eval a)
                                                            )
proofAnd eval a b | (not (calc a eval)) && (not (calc b eval)) = (t_e_arrow 
                                                                  (t_e_arrow 
                                                                    (ax9 (getContext eval) (Binary And a b) b) 
                                                                    (ax5 (getContext eval) a b))
                                                                  (t_e_arrow 
                                                                    (ax1 (getContext eval) (inv b) (Binary And a b))
                                                                    (proof  eval b)
                                                                  )
                                                                )
proofImpl :: Data.Map.Map String Bool -> Expr -> Expr -> Tree
proofImpl eval a b | (calc a eval) && (calc b eval) = (t_e_arrow 
                                                        (ax1 (getContext eval) b a)
                                                        (proof eval b)
                                                      )
proofImpl eval a b | (not (calc a eval)) && (calc b eval) = (t_e_arrow 
                                                              (ax1 (getContext eval) b a)
                                                              (proof eval b)
                                                            )
proofImpl eval a b | (calc a eval) && (not (calc b eval)) = (t_e_arrow
                                                              (t_e_arrow
                                                                (ax9 (getContext eval) (Binary Impl a b) b)
                                                                (t_e_arrow
                                                                  (t_i_arrow
                                                                    (t_i_arrow 
                                                                      (t_e_arrow
                                                                        (t_ax ([Binary Impl a b, a] ++ (getContext eval)) (Binary Impl a b))
                                                                        (t_ax ([Binary Impl a b, a] ++ (getContext eval)) a)
                                                                      )
                                                                    )
                                                                  )
                                                                  (proof eval a)
                                                                )
                                                              )
                                                              (t_e_arrow
                                                                (ax1 (getContext eval) (inv b) (Binary Impl a b))
                                                                (proof eval b)
                                                              )
                                                            )
proofImpl eval a b | (not (calc a eval)) && (not (calc b eval)) = (t_e_arrow
                                                                    (t_i_arrow
                                                                      (t_i_arrow
                                                                        (t_e_not
                                                                          (t_e_arrow
                                                                            (t_ax ([(inv b), a, (inv a)] ++ (getContext eval)) (inv a))
                                                                            (t_ax ([(inv b), a, (inv a)] ++ (getContext eval)) a)
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                    (proof eval a)
                                                                    )

contra :: Tree -> Tree
contra cur @ (Tree (ExprLine context (Binary Impl a b) _) _) = 
                                                          (t_i_arrow (t_i_arrow  (t_i_arrow
                                                          (t_e_arrow
                                                            (t_ax ([a, (inv b), (Binary Impl a b)] ++ context) (inv b))
                                                            (t_e_arrow
                                                              (t_ax ([a, (inv b), (Binary Impl a b)] ++ context) (Binary Impl a b))
                                                              (t_ax ([a, (inv b), (Binary Impl a b)] ++ context) a)
                                                            )
                                                          ))))
                                                          {-
                    (t_i_arrow (t_i_arrow (t_e_arrow
                      (t_e_arrow
                        (ax9 ([(inv b), (Binary Impl a b)] ++ context) a b)
                        (t_ax ([(inv b), (Binary Impl a b)] ++ context) (Binary Impl a b))
                      )
                      (t_e_arrow
                        (ax1 ([(inv b), (Binary Impl a b)] ++ context) (inv b) a)
                        (t_ax ([(inv b), (Binary Impl a b)] ++ context) (inv b))
                      )
                    )))-}

getContext :: Data.Map.Map String Bool -> [Expr]
getContext eval = (Data.List.map (\(a, b) -> (if (b) then (Var a) else (Binary Impl (Var a) FVal))) (reverse (Data.Map.toList eval)))

undoubleInv :: Tree -> Tree
undoubleInv cur @ (Tree (ExprLine context (Binary Impl (Binary Impl a FVal) FVal) _) _) = (t_e_arrow
                                                                                            (ax10 context a)
                                                                                            cur
                                                                                          )

joinRes :: Tree -> Tree -> Tree 
joinRes left @ (Tree (ExprLine context1 expr1 _) _) right @ (Tree (ExprLine context2 expr2 _) _) = 
  (undoubleInv
    (t_e_arrow
      (t_e_arrow
        (ax9 context (inv b) (inv a))
        (t_e_arrow
          (contra
            (t_i_arrow 
              left
            )
          )
          (t_i_arrow
            left
          )
        )
      )
      (t_e_arrow
        (contra
          (t_i_arrow
            right
          )
        )
        (t_i_arrow
          right
        )
      )
    )
    ) where a = (head context1)
            b = expr1
            context = (tail context1)

solve :: Expr -> IO () 
solve expr = do
  let vals = (collectWrapper expr)
  let generated = (generateValues vals [] Data.Map.empty)
  let eval = (Data.Map.fromList [("A", False), ("B", False)])
  --let eval2 = (Data.Map.fromList [("A", True), ("B", False)])
  let a1 = (Var "A")
  let b1 = (Var "B")
  let a = (Binary Impl b1 FVal) 
  let b = (Binary Impl a1 FVal)
  --let a1 = 
  --let b1 = 
  -- (A->B)->((A->(B->_|_))->(A->_|_))
  --let b = (Var "B")

  if (all (\v -> (calc expr v)) generated) then
    mshow (proofValues expr vals Data.Map.empty) 0
    --putStrLn (show (Data.List.map (\v -> (proof v expr)) generated))
  else 
    putStrLn ("Formula is refutable [" ++  (findFalse expr generated) ++ "]")
  

main :: IO ()
main = do
  line <- getLine
  case parseExpr (alexScanTokens line) of 
    Left err   -> do 
      putStrLn err
    Right val -> do
      solve val
  return ()
   