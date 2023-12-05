-- https://atcoder.jp/contests/abc303/tasks/abc303_c
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.Foldable
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Set as S
import Debug.Trace (traceShow)

type ByteString = B.ByteString
type Set = S.Set

-- {{ Parser
newtype Parser a = Parser (ByteString -> (a, ByteString))
instance Functor Parser where
    fmap f (Parser p) = Parser $ first f <$> p
instance Applicative Parser where
    pure a = Parser (a,)
    liftA2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
    liftA2 f (Parser pa) (Parser pb) = Parser $ pc . second pb . pa
      where
        pc (a, (b, bs)) = (f a b, bs)
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser pa >>= f = Parser $ (\(a, bs) -> let Parser p = f a in p bs) . pa

-- }}

-- {{ Serde
class Show a => Serde a where
    -- | serialise on one token
    ser :: a -> ByteString
    ser = B.pack . show

    -- | deserialise on one token
    des :: Parser a

    -- | deserialise on N tokens (only works on collections)
    desN :: Int -> Parser a

    -- | convert B.read to serialiser
    fromRead :: (ByteString -> Maybe (a, ByteString)) -> Parser a
    -- B.tail, assuming tokens are separated by \s
    fromRead r = Parser $ \bs -> let Just a = r bs in second B.tail a
instance (Serde a, Serde b) => Serde (a, b) where
    des = liftA2 (,) des des
instance Serde Bool where
    ser True = B.pack "Yes\n"
    ser False = B.pack "No\n"
instance Serde Integer where
    des = fromRead B.readInteger
instance Serde Int where
    des = fromRead B.readInt
instance Serde String where
    ser = B.pack
    des = Parser $ B.break isSpace >>> B.unpack *** B.tail
instance {-# OVERLAPS #-} Serde [String] where
    ser = B.unlines . fmap ser -- print [String] with \n
instance {-# OVERLAPS #-} Serde a => Serde [a] where
    ser = B.unwords . fmap ser
    desN = (`replicateM` des)
instance (Serde a, Ord a) => Serde (Set a) where
    desN = fmap S.fromList . desN

-- }}

-- {{ Debug

dbg :: Show a => a -> a
dbg x = let !_ = traceShow x () in x

-- | ($) with dbg
($$) :: Show a => (a -> b) -> a -> b
($$) f x = let !_ = dbg x in f x

infixr 0 $$

-- | (.) with dbg
(.$) :: Show b => (b -> c) -> (a -> b) -> a -> c
(g .$ f) x = let !y = dbg $ f x in g y

infixr 9 .$

-- }}

-- | tuple window
pairs :: [a] -> [(a, a)]
pairs = zip <*> tail

-- | tuple window with
pairW :: (a -> a -> b) -> [a] -> [b]
pairW = (<*> tail) . zipWith

type B = Bool
type C = Char
type I = Int
type S = String

main :: IO ()
main = B.interact $ ser . fst . p
  where
    Parser p = desN 4 >>= \[_, m, h, k] -> cc h k <$> des <*> desN m

cc :: I -> I -> S -> Set (I, I) -> B
cc h k = f k (0, 0) h

mv :: (I, I) -> C -> (I, I)
mv (x, y) 'R' = (x + 1, y)
mv (x, y) 'L' = (x - 1, y)
mv (x, y) 'U' = (x, y + 1)
mv (x, y) 'D' = (x, y - 1)

f :: I -> (I, I) -> I -> [C] -> Set (I, I) -> B
f _ _ _ [] _ = True
f k p h (c : ct) s
    | h' < 0 = False
    | h' < k && S.member p' s = f k p' k ct (S.delete p' s)
    | otherwise = f k p' h' ct s
  where
    p' = mv p c
    h' = h - 1
