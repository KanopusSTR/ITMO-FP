module HW3.T4
  ( State (..)
  , Prim (..)
  , Expr (..)
  , mapState
  , wrapState
  , joinState
  , modifyState
  , eval
  ) where

import HW3.T1

newtype State s a = S { runS :: s -> Annotated s a }

mapState :: (a -> b) -> State s a -> State s b
mapState f1 (S f2) = S (mapAnnotated f1 . f2)

wrapState :: a -> State s a
wrapState a = S (a :#)

joinState :: State s (State s a) -> State s a
joinState (S f) = S $ \s -> (let (S f2) :# e = f s in f2 e)

modifyState :: (s -> s) -> State s ()
modifyState f = S $ (() :#) . f

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  (<*>) (S f1) (S f2) = S $ \s ->
    let
      (a1 :# e1) = f1 s
      (a2 :# e2) = f2 e1 in (a1 a2 :# e2)

instance Monad (State s) where
  (>>=) st stFun = joinState (mapState stFun st)

data Prim a =
    Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a
  deriving Show

data Expr = Val Double | Op (Prim Expr)
  deriving Show

instance Num Expr where
  (+) a b = Op $ Add a b
  (-) a b = Op $ Sub a b
  (*) a b = Op $ Mul a b
  abs = Op . Abs
  signum = Op . Sgn
  fromInteger = Val . fromInteger

instance Fractional Expr where
  (/) a b = Op $ Div a b
  fromRational = Val . fromRational

eval :: Expr -> State [Prim Double] Double
eval (Val x) = pure x

eval (Op (Add a b)) = do
  f <- eval a
  l <- eval b
  modifyState (Add f l :)
  return $ f + l

eval (Op (Sub a b)) = do
  f <- eval a
  l <- eval b
  modifyState (Sub f l :)
  return $ f - l

eval (Op (Mul a b)) = do
  f <- eval a
  l <- eval b
  modifyState (Mul f l :)
  return $ f * l

eval (Op (Div a b)) = do
  f <- eval a
  l <- eval b
  modifyState (Div f l :)
  return $ f / l

eval (Op (Abs a)) = do
  f <- eval a
  modifyState (Abs f :)
  return $ abs f

eval (Op (Sgn a)) = do
  f <- eval a
  modifyState (Sgn f :)
  return $ signum f
