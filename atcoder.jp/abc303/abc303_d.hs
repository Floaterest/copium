-- https://atcoder.jp/contests/abc303/tasks/abc303_d
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
import qualified Data.HashMap.Lazy as M
import Data.List
import qualified Data.List.NonEmpty as N
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

type A = Array
type B = Bool
type C = Char
type M = M.HashMap
type I = Int
type S = Set

main :: IO ()
main = B.interact $ ser . fst . p
  where
    Parser p = dd <$> desN 3 <*> des

dd :: [I] -> [C] -> I
dd [a, s, c] cs = uncurry min $ last  as
  where
    at = min (a + c) (c + s)
    af = case head cs of
        'a' -> a
        'A' -> min s (c + a + c)
    as = (af, at) : zipWith f as (tail cs)
    ca = min (c + a) (s + c)
    cac = min s (c + a + c)
    csc = min a (c + s + c)
    f :: (I, I) -> C -> (I, I)
    f (off, on) 'a' = (min (off + csc) (on + ca), min (off + ca) (on + cac))
    f (off, on) 'A' = (min (off + cac) (on + ca), min (off + ca) (on + csc))
