module HW2.T4
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where


data ListPlus a = a :+ ListPlus a | Last a
  deriving Show

infixr 5 :+

instance Semigroup (ListPlus a) where
  (a :+ b) <> c = a :+ (b <> c)
  Last a <> b   = a :+ b

data Inclusive a b = This a | That b | Both a b
  deriving Show

-- You may necessary constraints there
instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b)
  where
  This a <> This b     = This $ a <> b
  This a <> That b     = Both a b
  That a <> This b     = Both b a
  This a <> Both b c   = Both (a <> b) c
  Both a b <> This c   = Both (a <> c) b
  That a <> That b     = That $ a <> b
  That a <> Both b c   = Both b (a <> c)
  Both a b <> That c   = Both a (b <> c)
  Both a b <> Both c d = Both (a <> c) (b <> d)

newtype DotString = DS String
  deriving Show

instance Semigroup DotString where
  ds <> DS ""    = ds
  DS "" <> ds    = ds
  DS s1 <> DS s2 = DS $ s1 ++ "." ++ s2
instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F a <> F b = F $ a . b

instance Monoid (Fun a) where
  mempty = F id
