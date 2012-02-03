{-# LANGUAGE Haskell2010, NoMonomorphismRestriction #-}

module Curry.Pivot (PivotStrategy, noPivot, rowPivot,
                    columnPivot, fullPivot, pivotize) where

import Control.Arrow ((***), (>>>), second)

-- import Data.Array (Array, assocs, bounds, ixmap)
import Data.Array.Unboxed (IArray, UArray, assocs, bounds, ixmap)
import Data.Tuple (swap)

import Curry.Arrows (both, join)
import Curry.Matrix (Matrix, column, maxAbsLoc, subMatrix,
                     transpose, transposition)

type PivotStrategy b = Matrix b -> (Int, Int) -> (Int, Int)

--noPivot :: PivotStrategy b
noPivot a = id

--rowPivot :: (Num b, Ord b) => PivotStrategy b
rowPivot a = second (transposition f i)
  where f = snd . fst . bounds $ a
        i = maxAbsLoc $ column f a

--columnPivot :: (Num b, Ord b) => PivotStrategy b
columnPivot = curry $ swap . (transpose *** swap >>> join rowPivot)

--fullPivot :: (Num b, Ord b) => PivotStrategy b
fullPivot a = transposition f i *** transposition g j
  where (f, g) = fst . bounds $ a
        (i, j) = maxAbsLoc a

--pivotize :: PivotStrategy b -> Matrix b -> Matrix b
pivotize p a = go 1 a id
               where bnds = bounds a
                     m = fst . snd $ bnds
                     go i a s
                       | i <= m = go (i + 1) a' s'
                       | otherwise = a
                       where s' = p $ subMatrix i i a
                             a' = ixmap bnds s' a