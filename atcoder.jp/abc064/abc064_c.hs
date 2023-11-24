-- https://atcoder.jp/contests/abc064/tasks/abc064_c
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe

type ByteString = BS.ByteString

newtype Parser a = Parser ([ByteString] -> Maybe (a, [ByteString]))

instance Functor Parser where
    fmap f (Parser p) = Parser $ (first f <$>) . p

instance Applicative Parser where
    pure a = Parser $ fmap (a,) . Just
    liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    liftA2 f (Parser pa) (Parser pb) = Parser $ pa >=> pc
      where
        pc (a, bs) = (<$> pb bs) $ first $ f a

instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser pa >>= f = Parser $ pa >=> pb
      where
        pb (a, bs) = let (Parser p) = f a in p bs

parse :: Parser a -> ByteString -> a
-- split BS into words, then unwrap result of parse
parse (Parser p) = (\(Just res) -> fst res) . p . BS.words

next :: Parser ByteString
next = Parser uncons

int :: Parser Integer
int = (\(Just res) -> fst res) . BS.readInteger <$> next

ints :: Parser [Integer]
ints = Parser $ \bs -> Just (p bs, [])
  where
    p bs = bs >>= maybeToList . fmap fst . BS.readInteger

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f

main :: IO ()
-- main = BS.getContents >>= putStrLn . tostr . parse p
main = BS.interact $ tostr . parse p
  where
    p = cc <$> ints
    tostr = BS.pack . unwords . fmap show

cc :: [Integer] -> [Int]
cc (_ : ns) = [max 1 a, a + b]
  where
    as = (`div` 400) <$> ns
    b = count (>= 8) as
    a = (length . group . sort . filter (< 8)) as
