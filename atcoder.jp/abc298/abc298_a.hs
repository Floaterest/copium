-- https://atcoder.jp/contests/abc298/tasks/abc298_a
import Data.List
import Data.Maybe

yes :: Bool -> String
yes True = "Yes"
yes False = "No"

main :: IO ()
main = interact $ solve . words

solve :: [String] -> String
solve [_, s] = yes $ isJust o && isNothing x
  where
    o = elemIndex 'o' s
    x = elemIndex 'x' s
