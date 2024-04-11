module HW2.T2
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty ((:|)), toList)

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn delim (f:rest)
  | delim == f = [] :| toList splitted
  | otherwise = (f:fIn) :| restIn
  where splitted@(fIn :| restIn) = splitOn delim rest
splitOn _ [] = [] :| []

joinWith :: a -> NonEmpty [a] -> [a]
joinWith _ (f :| [])           = f
joinWith delim (f :| (s:rest)) = f ++ [delim] ++ joinWith delim (s :| rest)
