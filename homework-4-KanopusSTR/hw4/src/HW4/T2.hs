{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}

module HW4.T2
  ( ParseError (..)
  , runP
  , pChar
  , parseError
  , parseExpr
  , empty
  , (<|>)
  , pAbbr
  ) where

import Control.Applicative
import Control.Monad
import Numeric.Natural (Natural)

import Data.Char (digitToInt, isDigit)
import qualified Data.Char
import Data.Ratio ((%))
import HW4.T1 (ExceptState (..))
import HW4.Types

newtype ParseError = ErrorAtPos Natural
  deriving Show

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving newtype (Functor, Applicative, Monad)

runP :: Parser a -> String -> Except ParseError a
runP parser@(P (ES f)) str = case f (0, str) of
  Error (ErrorAtPos n)       -> Error (ErrorAtPos n)
  Success (a :# (_, []))     -> Success a
  Success (_ :# (_, _:str2)) -> runP parser str2

-- Just an example of parser that may be useful
-- in the implementation of 'parseExpr'
pChar :: Parser Char
pChar = P $ ES $ \(pos, s) ->
  case s of
    []     -> Error (ErrorAtPos pos)
    (c:cs) -> Success (c :# (pos + 1, cs))

parseError :: Parser a
parseError = P $ ES $ \(pos, _) -> Error (ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P p1) (P p2) = P (ES (\(pos, s) ->
    case runES p1 (pos, s) of
      Error (ErrorAtPos _) -> runES p2 (pos, s)
      Success _            -> runES p1 (pos, s)))

pEof :: Parser ()
pEof = P $ ES $ \(pos, s) -> case s of
    [] -> Success (() :# (pos, s))
    _  -> Error (ErrorAtPos pos)

pAbbr :: Parser String
pAbbr = do
  abbr <- some (mfilter Data.Char.isUpper pChar)
  pEof
  pure abbr

-- No metohds
instance MonadPlus Parser

toNumber :: [Char] -> Integer
toNumber = (\x y -> x * 10 + fromIntegral (digitToInt y)) `foldl` 0

myFilter :: Char -> Parser Char
myFilter ch = mfilter (== ch) pChar

pNumber :: Parser Expr
pNumber = do
  unMinus <- fmap (\case
    Nothing -> 1
    Just _ -> -1)
    (optional (myFilter '-'))

  nPart1 <- some $ mfilter isDigit pChar
  _ <- optional $ myFilter '.'
  nPart2 <- many $ mfilter isDigit pChar
  pure (Val $ fromRational (unMinus * toNumber (nPart1 ++ nPart2) % (10 ^ length nPart2)))

pSpace :: Parser ()
pSpace = void $ many (myFilter ' ')

pExpr :: Parser Expr
pExpr = do
  res <- pExpr1
  pEof
  pure res

pExpr1 :: Parser Expr
pExpr1 = pExpr1' <|> pExpr2

pExpr1' :: Parser Expr
pExpr1' = do
  res1 <- pExpr2
  sign <- pSpace *> (myFilter '+' <|> myFilter '-') <* pSpace
  res2 <- pExpr1
  pure (if sign == '+' then res1 + res2 else res1 - res2)


pExpr2 :: Parser Expr
pExpr2 = pExpr2' <|> pExpr3

pExpr2' :: Parser Expr
pExpr2' = do
  res1 <- pExpr3
  sign <- pSpace *> (myFilter '*' <|> myFilter '/') <* pSpace
  res2 <- pExpr2
  pure (if sign == '*' then res1 * res2 else res1 / res2)

pExpr3 :: Parser Expr
pExpr3 = pExpr3' <|> pSpace *> pNumber <* pSpace

pExpr3' :: Parser Expr
pExpr3' = do
  _ <- pSpace *> myFilter '(' <* pSpace
  res1 <- pSpace *> pExpr1
  _ <- pSpace *> myFilter ')' <* pSpace
  pure res1


parseExpr :: String -> Except ParseError Expr
parseExpr = runP pExpr
