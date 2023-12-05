{-
Module     :  Main
Description:  Advent of Code 2023 - Day 01
License    :  MIT
Maintainer :  Skoett@Github

Day 1: Fix the calibration document

-}

module Main (main) where
import Data.Char (isDigit)
import Data.List
import Data.Maybe

main :: IO ()
main = do
   input <- readFile "../input/day_01_3_test.txt"
   let l = lines input
   print $ dayOneTaskOne l
   print $ dayOneTaskTwo l


dayOneTaskOne :: [String] -> Int
dayOneTaskOne l = sum $ map findDigit l

dayOneTaskTwo :: [String] -> Int
dayOneTaskTwo l = sum $ map findDigit' l

findDigit :: String -> Int
findDigit s = read $ take 1 f ++ take 1 (reverse f)
               where f = filter isDigit s

findDigit' :: String -> Int
findDigit' s = let x = findStringIndex s
                   (min, i) = minimumBy ord x
                   (max, j) = maximumBy ord x
               in read $ min ++ max
               where ord (_, x) (_, y) = compare x y


findDigit'' :: [Char] -> IO ()
findDigit'' s = let x = findStringIndex s
                    (min, i) = minimumBy ord x
                    (max, j) = maximumBy ord x
                in print x
                where ord (_, x) (_, y) = compare x y

findStringIndex :: String -> [(String, Int)]
findStringIndex s = let x = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
                        y = map show [1,2..9]
                     in map mapStringIndex $ concatMap (\x -> findIndex' x s 0) (x ++ y)

mapStringIndex :: (String, Int) -> (String, Int)
mapStringIndex (s, i) | s == "one"   = ("1", i)
                      | s == "two"   = ("2", i)
                      | s == "three" = ("3", i)
                      | s == "four"  = ("4", i)
                      | s == "five"  = ("5", i)
                      | s == "six"   = ("6", i)
                      | s == "seven" = ("7", i)
                      | s == "eight" = ("8", i)
                      | s == "nine"  = ("9", i)
                      | otherwise    = (s, i)

findIndex' :: String -> String -> Int -> [(String, Int)]
findIndex' sub string offset = case findIndex (isPrefixOf sub) (tails string) of
                    Just x -> (sub, offset + x) : findIndex' sub (drop (x + l) string) (offset + (x + l))
                    Nothing -> []
                    where l = if length sub == 1 then 1 else length sub - 1
