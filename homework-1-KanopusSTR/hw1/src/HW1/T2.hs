module HW1.T2
  ( N (..)
  , nplus
  , nmult
  , nsub
  , nFromNatural
  , nToNum
  , ncmp
  , nEven
  , nOdd
  , ndiv
  , nmod
  ) where

import Data.Maybe (fromMaybe)
import Numeric.Natural

data N = Z | S N

nplus :: N -> N -> N
nplus x Z     = x
nplus x (S y) = S $ nplus x y

nmult :: N -> N -> N
nmult _ Z     = Z
nmult x (S y) = nmult x y `nplus` x

nsub :: N -> N -> Maybe N
nsub x Z         = Just x
nsub Z _         = Nothing
nsub (S x) (S y) = nsub x y

ncmp :: N -> N -> Ordering
ncmp Z Z         = EQ
ncmp _ Z         = GT
ncmp Z _         = LT
ncmp (S x) (S y) = ncmp x y

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural x = S $ nFromNatural $ x - 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S x) = nToNum x + 1

nEven :: N -> Bool
nEven Z         = True
nEven (S Z)     = False
nEven (S (S x)) = nEven x

nOdd :: N -> Bool
nOdd x = nEven $ S x

ndiv :: N -> N -> N
ndiv x y =
  if ncmp x y == LT then Z
  else S $ fromMaybe Z (nsub x y) `ndiv` y

nmod :: N -> N -> N
nmod x y = fromMaybe Z $ nsub x $ ndiv x y `nmult` y
