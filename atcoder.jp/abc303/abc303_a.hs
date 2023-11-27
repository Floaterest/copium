-- https://atcoder.jp/contests/abc303/tasks/abc303_a
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# HLINT ignore "Use infix" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-unrecognised-pragmas -Wno-unused-imports -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-missing-methods #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Array
import qualified Data.ByteString.Char8 as BS
import Data.Char
import Data.Foldable
import Data.List
import Data.Maybe
import Prelude hiding (read)

type ByteString = BS.ByteString

newtype Parser a = Parser ([ByteString] -> (a, [ByteString]))

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

class Readable a where
    next :: Parser a
    next = Parser $ \(b : bt) -> let Just (a, _) = read b in (a, bt)
    read :: ByteString -> Maybe (a, ByteString)

instance Readable Integer where
    read = BS.readInteger

instance {-# OVERLAPS #-} Readable String where
    next = Parser $ \bs -> let Just (b, bt) = uncons bs in (BS.unpack b, bt)
    read = Just . (,BS.empty) . BS.unpack

instance Readable a => Readable [a] where
    next = Parser $ (>>= maybeToList . fmap fst . read) >>> (,[])

class ToString a where
    tostr :: a -> ByteString

instance {-# OVERLAPS #-} ToString Bool where
    tostr True = BS.pack "Yes\n"
    tostr False = BS.pack "No\n"

instance (Show a, Num a) => ToString a where
    tostr = BS.pack . show

instance {-# OVERLAPS #-} ToString a => ToString [a] where
    tostr = BS.unwords . fmap tostr

main :: IO ()
main = BS.interact $ tostr . fst . p . BS.words
  where
    Parser p = aa <$> next <*> next

aa :: Integer -> [String] -> Bool
aa _ [a, b] = (f <$> a) == (f <$> b)
  where
    f '0' = 'o'
    f '1' = 'l'
    f c = c
