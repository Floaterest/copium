-- https://atcoder.jp/contests/abc299/tasks/abc299_a
import Data.List
import Data.Maybe

main :: IO ()
-- main = interact $ show . solve . (<$>) read . words
main = interact $ solve . words

solve :: [String] -> String
solve [_, s] = y $ a < b && b < c
  where
    [a, c] = elemIndices '|' s
    b = fromMaybe 0 $ elemIndex '*' s

y :: Bool -> String
y True = "in"
y False = "out"
