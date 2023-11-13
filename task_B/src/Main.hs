module Main where

import Grammar (Line (..), Context (..), Binop (..), Expr (..))
import Lexer (alexScanTokens)
import Parser (parseExpr)
import Data.List
import Data.Map
import Data.Maybe
import System.IO(isEOF)

matches :: Line -> Integer
matches (Line _ (Binary Impl a1 (Binary Impl b a2))) | a1 == a2 = 1
matches (Line _ (Binary Impl (Binary Impl a1 b1) (Binary Impl (Binary Impl a2 (Binary Impl b2 c1)) (Binary Impl a3 c2)))) | a1 == a2 && a2 == a3 && b1 == b2 && c1 == c2 = 2
matches (Line _ (Binary Impl a1 (Binary Impl b1 (Binary And a2 b2)))) | (a1 == a2) && (b1 == b2) = 3
matches (Line _ (Binary Impl (Binary And a1 _) a2)) | a1 == a2 = 4
matches (Line _ (Binary Impl (Binary And _ b1) b2)) | b1 == b2 = 5
matches (Line _ (Binary Impl a1 (Binary Or a2 _))) | a1 == a2 = 6
matches (Line _ (Binary Impl b1 (Binary Or _ b2))) | b1 == b2 = 7
matches (Line _ (Binary Impl (Binary Impl a1 c1) (Binary Impl (Binary Impl b1 c2) (Binary Impl (Binary Or a2 b2) c3)))) | a1 == a2 && b1 == b2 && c1 == c2 && c2 == c3 = 8
matches (Line _ (Binary Impl (Binary Impl a1 b1) (Binary Impl (Binary Impl a2 (Not b2)) (Not a3)))) | a1 == a2 && a2 == a3 && b1 == b2 = 9
matches (Line _ (Binary Impl (Not (Not a1)) a2)) | a1 == a2 = 10
matches val = 0


findDeduction :: Data.Map.Map Line Integer -> Line -> Integer
findDeduction deductions line | (Data.Map.member reformed deductions) = fromJust $ (Data.Map.lookup reformed deductions)
  where reformed = (reformDeduction line)
findDeduction deductions line = 0


reformDeduction :: Line -> Line
reformDeduction (Line (Context left) (Binary Impl a b)) = (reformDeduction (Line (Context (a : left)) b))
reformDeduction (Line (Context left) line) = (Line (Context (sort left)) line)

handleDeduction :: Data.Map.Map Line Integer -> [Line] -> Line -> String
handleDeduction deductions parsed line | (deduction /= 0) = ("[Ded. " ++ (show deduction) ++ "]")
                            | (deduction == 0) = "[Incorrect]"
                            where deduction = (findDeduction deductions line)

findMP :: [Line] -> Line -> Data.Map.Map Expr Integer -> Integer -> (Integer, Integer)
findMP [] line map num = (0, 0) 
findMP ((Line (Context context1) line1) : parsed) (Line (Context context2) line2) map num | (context1 == context2 && (member line1 map)) = (num, (fromJust $ Data.Map.lookup line1 map))
findMP (cur : parsed) line map num = (findMP parsed line map (num + 1))

fillMpMap :: [Line] -> Line -> Data.Map.Map Expr Integer -> Integer -> Data.Map.Map Expr Integer
fillMpMap [] line map num = map
fillMpMap ((Line (Context context1) (Binary Impl a b)) : lines) (Line (Context context2) line2) map num | (context1 == context2 && b == line2) = (fillMpMap lines (Line (Context context2) line2) (Data.Map.insert a num map) (num + 1))
fillMpMap (cur : parsed) line map num = (fillMpMap parsed line map (num + 1))


handleMP :: Data.Map.Map Line Integer ->  [Line] -> Line -> String
handleMP deductions parsed line | ((a, b) /= (0, 0)) = ("[M.P. " ++ (show a) ++ ", " ++ (show b) ++ "]")
                      | ((a, b) == (0, 0)) = (handleDeduction deductions parsed line)
                      where (a, b) = (findMP parsed line (fillMpMap parsed line Data.Map.empty 1) 1)

hypotis :: Line -> Integer
hypotis (Line (Context context) expr) | (elem expr context) = ((toInteger $ fromJust $ elemIndex expr context) + 1)
hypotis other = 0

handleHypotis :: Data.Map.Map Line Integer ->  [Line] -> Line -> String
handleHypotis deductions parsed line | (hypot == 0) = (handleMP deductions parsed (reorder line))
                    | (hypot /= 0) = ("[Hyp. " ++ (show hypot) ++ "]")
                    where hypot = (hypotis line)


handle :: Data.Map.Map Line Integer ->  [Line] -> Line -> String
handle deductions parsed line | (match /= 0) = ("[Ax. sch. " ++ (show match) ++ "]")
               | (match == 0) = (handleHypotis deductions parsed line)
               where match = (matches line)

readAll :: [Line] -> Integer -> IO [Line]
readAll result index = do
  done <- isEOF
  if done 
    then return result
    else do
      line <- getLine
      case parseExpr (alexScanTokens line) of 
        Left err   -> do 
          putStrLn err
          return []
        Right val -> do
          ans <- (readAll (val : result) (index + 1))
          return ans

reorder :: Line ->  Line 
reorder (Line (Context context) line) = (Line (Context (sort context)) line)

handleData :: [Line] -> [Line] -> Integer  -> Map Line Integer -> IO()
handleData parsed [] num deductions = return()
handleData parsed (line:lines) num deductions = do
  putStrLn("[" ++ (show num) ++ "] " ++ (show line) ++ " " ++ (handle deductions parsed line))
  if (Data.Map.member reformed deductions) then
    handleData (parsed ++ [(reorder line)]) lines (num + 1) deductions
    else handleData (parsed ++ [(reorder line)]) lines (num + 1) (Data.Map.insert reformed num deductions)
  where reformed = (reformDeduction line)

main :: IO ()
main = do
  val <- readAll [] 1
  handleData [] (reverse val) 1 Data.Map.empty
  return ()
  