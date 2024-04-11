{-# LANGUAGE LambdaCase #-}
module HW2.T3
  ( epart
  , mcat
  ) where
import Data.Foldable

mcat :: Monoid a => [Maybe a] -> a
mcat = foldr (mappend . fold) (fold Nothing)

epart :: (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldMap (\case
  Left type1  -> (type1, mempty)
  Right type2 -> (mempty, type2))
