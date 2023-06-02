-- https://atcoder.jp/contests/abc300/tasks/abc300_a
import Data.List

main :: IO ()
main= interact $ show . solve . (<$>) read . words

solve :: [Integer] -> Int
solve (_ : a : b : cs) = maybe 0 (+ 1) $ elemIndex (a + b) cs
