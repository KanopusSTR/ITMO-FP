module HW3.T3
  ( joinOption
  , joinExcept
  , joinAnnotated
  , joinList
  , joinFun
  ) where

import HW3.T1

joinOption :: Option (Option a) -> Option a
joinOption (Some (Some a)) = Some a
joinOption _               = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success (Success a)) =  Success a
joinExcept (Error e)             =  Error e
joinExcept (Success (Error e))   =  Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a1 :# e1) :# e2) = a1 :# (e2 <> e1)

joinList :: List (List a) -> List a
joinList ((first:. lst1) :. lst2) = first :. joinList (lst1 :. lst2)
joinList (Nil :. lst)             = joinList lst
joinList Nil                      = Nil

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ \i -> (let F f2 = f i in f2 i)
