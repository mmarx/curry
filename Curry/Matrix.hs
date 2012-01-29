{-# LANGUAGE Haskell2010, FlexibleContexts, FlexibleInstances, TupleSections, TypeSynonymInstances #-}

module Curry.Matrix where

import Control.Arrow ((***), (>>>), (&&&), first, second)
import Control.Monad (when)

import Data.Tuple (swap)
import Data.Array (Array, (!), (//),
                   accum, array, assocs, bounds, elems, ixmap,
                   listArray)
import Data.Function (on)
import Data.Ix (Ix)
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)

import Curry.Arrows (split, join, both)

type Vector b = Array Int b
type Matrix b = Array (Int, Int) b

fromScalar :: Num b => Int -> Int -> b -> Matrix b
fromScalar n m v = listArray ((1, 1), (m, n)) $ replicate (n * m) v

zeros n m = fromScalar n m 0
ones n m = fromScalar n m 1
eye n = zeros n n // [((i, i), 1) | i <- [1 .. n]]

hilb :: Int -> Matrix Rational
hilb n = listArray ((1, 1), (n, n)) [1.0 / toRational (i + j - 1)
                                       | i <- [1 .. n],
                                         j <- [1 .. n]]

withBoundsCheck chk op l r | chk (bounds l) (bounds r) = op l r
                           | otherwise = error "Non-conforming array dimensions"

liftOp :: (Num a, Num b, Ix i) => (a -> b -> a) -> Array i a -> Array i b -> Array i a
liftOp o = withBoundsCheck (==) (\l r -> accum o l $ assocs r)

instance Num b => Num (Vector b) where
  (+) = liftOp (+)
  (*) = liftOp (*)
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = array (1, 1) . (:[]) . (1,) . fromInteger

instance Num b => Num (Matrix b) where
  (+) = liftOp (+)
  (*) = withBoundsCheck chk matmul
    where chk = curry $ both fst *** both snd >>> join (==)
          matmul l r = listArray bnds [sum [(l ! (k, i)) * (r ! (j, k))
                                                       | k <- [d' .. d]]
                                         | i <- [m' .. m],
                                           j <- [n' .. n]]
            where bnds = both bounds >>> both snd *** both fst >>>
                         both fst &&& both snd $ (l, r)
                  (d', d) = both fst $ bounds l
                  ((m', n'), (m, n)) = bnds
  abs = fmap abs
  negate = fmap negate
  signum = fmap signum
  fromInteger = fromScalar 1 1 . fromInteger

transpose :: Matrix b -> Matrix b
transpose a = ixmap (both swap $ bounds a) (\(i, j) -> (j, i)) a

dual :: (Matrix b -> Matrix b) -> Matrix b -> Matrix b
dual f = transpose . f . transpose

isLowerTriangular :: Num b => Matrix b -> Bool
isLowerTriangular a = all (0==) [a ! (i, j) | i <- [m', m], j <- [n' .. n], j > i]
  where ((m', n'), (m, n)) = bounds a

isSquare :: Matrix b -> Bool
isSquare = bounds >>> both fst &&& both snd >>> both sub >>> join (==)
  where sub = join (-)

isDiagonal :: Num b => Matrix b -> Bool
isDiagonal a = all (0==) [a ! (i, j) | i <- [m' .. m], j <- [n' .. n], i /= j]
  where ((m', n'), (m, n)) = bounds a

isTriangular :: (Num b, Eq b) => Matrix b -> Bool
isTriangular = split >>> second transpose >>>
               both isLowerTriangular >>> join (||)

isSingular :: (Num b, Eq b) => Matrix b -> Bool
isSingular = any (0==) . elems . diagonal

transposition :: Int -> Int -> Int -> Int
transposition i j m
  | m == i = j
  | m == j = i
  | otherwise = m

swapRows :: Int -> Int -> Matrix b -> Matrix b
swapRows i j a = ixmap (bounds a) (second $ transposition i j) a

swapColumns :: Int -> Int -> Matrix b -> Matrix b
swapColumns i j = dual $ swapRows i j

diagonal :: Matrix b -> Vector b
diagonal a
  | isSquare a = array bnds fltr
  | otherwise = error "Matrix is not suare"
  where bnds = bounds >>> both fst $ a
        fltr = assocs >>> filter (uncurry (==) . fst) >>> map (first fst) $ a

row :: Int -> Matrix b -> Vector b
row i = split >>> bnds *** fltr >>> join array
  where bnds = bounds >>> both fst
        fltr = assocs >>> filter ((i==) . snd . fst) >>> map (first fst)

column :: Int -> Matrix b -> Vector b
column i = row i . transpose

rows :: Matrix b -> [Int]
rows = bounds >>> both snd >>> join enumFromTo

columns :: Matrix b -> [Int]
columns = rows . transpose

subMatrix :: Int -> Int -> Matrix b -> Matrix b
subMatrix i j a = ixmap (first (((j - n)+) *** ((i - m)+)) $ bounds a) id a
  where (n, m) = fst . bounds $ a

foldRows :: (Vector b -> Vector b -> Vector b) -> Vector b -> Matrix b -> Vector b
foldRows f x a = foldr (f . flip row a) x $ rows a

foldRows1 f a = foldRows f x a'
  where x = split >>> first (last . rows) >>> join row $ a
        a' = ixmap (first (first succ) $ bounds a) id a

maxLocBy :: (Ord e, Ix i) => (e -> e -> Ordering) -> Array i e -> i
maxLocBy cmp = fst . maximumBy (cmp `on` snd) . assocs

maxAbsLoc :: (Ord e, Num e, Ix i) => Array i e -> i
maxAbsLoc = maxLocBy (comparing abs)

prettyPrintVector :: Show b => Vector b -> IO ()
prettyPrintVector a = do prettyPrintVector' a
                         putStrLn ""

prettyPrintVector' :: Show b => Vector b -> IO ()
prettyPrintVector' a = mapM_ putStr ["[",
                                     intercalate ", " (map show (elems a)),
                                     "]"]

prettyPrintMatrix :: Show b => Matrix b -> IO ()
prettyPrintMatrix a = do putStr "["
                         mapM_ printRow rws
                         putStrLn "]"
  where rws = rows a
        printRow r = do when (r /= head rws) $ putStr " "
                        prettyPrintVector' $ row r a
                        when (r /= last rws) $ putStrLn ","