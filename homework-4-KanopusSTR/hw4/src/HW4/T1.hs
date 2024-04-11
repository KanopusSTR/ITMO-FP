module HW4.T1
  ( EvaluationError (..)
  , ExceptState (..)
  , mapExceptState
  , wrapExceptState
  , joinExceptState
  , modifyExceptState
  , throwExceptState
  , eval
  ) where

import Control.Monad (ap)
import HW4.Types

newtype ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f1 (ES f2) = ES $ mapExcept (mapAnnotated f1) . f2

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ Success . (:#) a

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES $ \s1 ->
  case f s1 of
    Success (ES f2 :# s2) ->  f2 s2
    Error e               -> Error e

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ Success . (:#) () . f

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  (<*>) = ap

instance Monad (ExceptState e s) where
  (>>=) est estFun = joinExceptState (mapExceptState estFun est)

data EvaluationError = DivideByZero
  deriving Show

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = pure x

eval (Op (Add a b)) = do
  f <- eval a
  l <- eval b
  modifyExceptState (Add f l :)
  return $ f + l

eval (Op (Sub a b)) = do
  f <- eval a
  l <- eval b
  modifyExceptState (Sub f l :)
  return $ f - l

eval (Op (Mul a b)) = do
  f <- eval a
  l <- eval b
  modifyExceptState (Mul f l :)
  return $ f * l

eval (Op (Div a b)) = do
  f <- eval a
  l <- eval b
  if l == 0.0 then throwExceptState DivideByZero
  else modifyExceptState (Div f l :)
  return $ f / l

eval (Op (Abs a)) = do
  f <- eval a
  modifyExceptState (Abs f :)
  return $ abs f

eval (Op (Sgn a)) = do
  f <- eval a
  modifyExceptState (Sgn f :)
  return $ signum f
