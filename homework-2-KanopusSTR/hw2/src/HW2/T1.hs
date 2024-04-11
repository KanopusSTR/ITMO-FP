module HW2.T1
  ( Tree (..)
  , tfoldr
  ) where

data Tree a = Leaf | Branch !Int (Tree a) a (Tree a)
  deriving (Show)

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr func cont (Branch _ treeA val treeB) = tfoldr func (func val $ tfoldr func cont treeB) treeA
tfoldr _ cont Leaf                          = cont
