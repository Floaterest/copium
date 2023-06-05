-- https://atcoder.jp/contests/abc300/tasks/abc300_b
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use infix" #-}
import Data.List
import Data.Maybe

yes :: Bool -> String
yes True = "Yes"
yes False = "No"

main :: IO ()
main = interact $ yes . solve . lines

solve :: [String] -> Bool
solve (hw : ab) = elem b as
  where
    as = [d | c <- rotate h a, d <- transpose $ map (rotate w) c]
    [h, w] = read <$> words hw
    (a, b) = splitAt h ab

rotate :: Int -> [a] -> [[a]]
rotate n = take n . (map . take) n . tails . cycle
