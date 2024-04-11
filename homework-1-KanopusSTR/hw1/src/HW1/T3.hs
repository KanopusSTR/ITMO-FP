module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

type Meta = (Int, Int)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                  = 0
tsize (Branch (a, _) _ _ _) = a

tdepth :: Tree a -> Int
tdepth Leaf                  = 0
tdepth (Branch (_, a) _ _ _) = a

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf             = False
tmember x (Branch _ a b c) = (x == b) || (if x < b then tmember x a else tmember x c)

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert x  Leaf = mkBranch Leaf x Leaf
tinsert x tree@(Branch _ a b c)
  | x < b     = balance $ mkBranch (tinsert x a) b c
  | x == b    = tree
  | otherwise = balance $ mkBranch a b (tinsert x c)

tFromList :: Ord a => [a] -> Tree a
tFromList = flip tinsert `foldl` Leaf

balance :: Tree a -> Tree a
balance Leaf = Leaf
balance a@(Branch _ left _ right)
  | balanceFactor a == -2 =
    if balanceFactor right == 1
      then bigLeft a
      else smallLeft a
  | balanceFactor a == 2 =
    if balanceFactor left == -1
      then bigRight a
      else smallRight a
  | otherwise = a

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch tree1 v tree2 = Branch (tsize tree1 + tsize tree2 + 1, 1 + (tdepth tree1 `max` tdepth tree2)) tree1 v tree2

balanceFactor :: Tree a -> Int
balanceFactor Leaf                            = 0
balanceFactor (Branch _ leftTree _ rightTree) = tdepth leftTree - tdepth rightTree

smallLeft :: Tree a -> Tree a
smallLeft (Branch _ l value (Branch _ rl rValue rr)) = mkBranch (mkBranch l value rl) rValue rr
smallLeft _                                          = Leaf

bigLeft :: Tree a -> Tree a
bigLeft (Branch _ l value (Branch _ (Branch _ rll rlValue rlr) rValue rr)) = mkBranch (mkBranch l value rll) rlValue (mkBranch rlr rValue rr)
bigLeft _                                                                  = Leaf

smallRight :: Tree a -> Tree a
smallRight (Branch _ (Branch _ ll lValue lr) value r) = mkBranch ll lValue (mkBranch lr value r)
smallRight _                                          = Leaf

bigRight :: Tree a -> Tree a
bigRight (Branch _ (Branch _ ll lValue (Branch _ lrl lrValue lrr)) value r) = mkBranch (mkBranch ll lValue lrl) lrValue (mkBranch lrr value r)
bigRight _                                                                  = Leaf
