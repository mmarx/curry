{-# LANGUAGE Haskell2010 #-}

module Curry.Gauss where

import Data.Array (accum, assocs, bounds, (!), (//))

import Curry.Matrix (Matrix, isSquare, isSingular, zeros)

luDecomposition :: (Num b, Eq b, Fractional b) => Matrix b -> (Matrix b, Matrix b)
luDecomposition a
  | i /= j || (not . isSquare $ a) = error "Cannot decompose non-square matrix"
  | isSingular a = error "Cannot decompose singular matrix"
  | otherwise = go i i a
  where ((i, j), (m, _)) = bounds a
        go i j a
          | i <= m && j < i = go i (j + 1) a
          | i <= m && j == i = go i (j + 1) a'
          | i <= m && j <= m = go i (j + 1) a'''
          | i <= m && j > m = go (i + 1) 1 a
          | otherwise = (l, u)
          where a' = accum (-) a [((j, i),
                                   a ! (k, i) * a ! (j, k))
                                 | k <- [1 .. i - 1]]
                a'' = accum (-) a' [((i, j),
                                     a ! (k, j) * a ! (i, k))
                                   | k <- [1 .. i - 1]]
                a''' = accum (/) a'' [((i, j),
                                       a'' ! (i, i))]
                triangle ord = filter (uncurry ord . fst) . assocs
                l = zeros m m // ([((i, i), 1) | i <- [1 .. m]] ++
                                  triangle (<) a)
                u = zeros m m // triangle (>=) a
