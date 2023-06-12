{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unqualified-imports -Wno-unused-imports -Wno-unused-top-binds #-}

import Control.Arrow hiding (arr)
import Data.List
import Debug.Trace

type I = Integer
type S = String
type C = Char

main :: IO ()
main = interact $ unwords . solve . words
  where
    solve :: [S] -> [S]
    solve = id
