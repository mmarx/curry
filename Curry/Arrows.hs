{-# LANGUAGE Haskell2010 #-}

module Curry.Arrows (split, join, both) where

import Control.Arrow (Arrow, arr, (***))

split :: Arrow a => a b (b, b)
split = arr $ \x -> (x, x)

join :: Arrow a => (b -> c -> d) -> a (b, c) d
join = arr . uncurry

both f = f *** f