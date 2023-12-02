-- https://atcoder.jp/contests/abc303/tasks/abc303_c
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

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
    ser :: a -> ByteString
    ser = B.pack . show
    des :: Parser a
    desn :: Int -> Parser [a]
    desn n = replicateM n des

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
    des = Parser $ unfoldr f >>> (,B.empty)
      where
        f bs | B.null bs = Nothing
        f bs = let Parser p = des in Just $ p bs

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

-- | tupli window with
pairW :: (a -> a -> b) -> [a] -> [b]
pairW = (<*> tail) . zipWith

type B = Bool
type I = Int
type S = String

main :: IO ()
main = B.interact $ ser . fst . p
  where
    Parser p = desn 4 >>= \[_, m, h, k] -> des >>= (<$> replicateM m des) . cc h k

cc :: I -> I -> S -> [(I, I)] -> B
cc h' k s' ps' = hh >= 0
  where
    g (x, y, h, s)
        | h < 0 = (x, y, -1, s)
        | S.member (x, y) s && h < k = (x, y, k, S.delete (x, y) s)
        | otherwise = (x, y, h, s)
    f (x, y, h, s) 'R' = g (x + 1, y, h - 1, s)
    f (x, y, h, s) 'L' = g (x - 1, y, h - 1, s)
    f (x, y, h, s) 'U' = g (x, y + 1, h - 1, s)
    f (x, y, h, s) 'D' = g (x, y - 1, h - 1, s)
    (_, _, hh, _) = foldl f (0, 0, h', S.fromList ps') s'
