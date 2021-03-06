module LCS where

import Prelude (class Eq, ($), (==), (>))
import Data.List (List(..), (:))
import Data.List as L
import Data.Array as A
import Data.Function.Memoize

lcs :: forall a. Eq a => Tabulate a => Array a -> Array a -> Array a
lcs a b = A.fromFoldable $
            (memoize2 lcsa)
            (L.fromFoldable a)
            (L.fromFoldable b)

lcsa :: forall a. Eq a => List a -> List a -> List a
lcsa Nil _ = Nil
lcsa _ Nil = Nil
lcsa (a:as) (b:bs) | a == b = a : lcsa as bs
lcsa as@(_:as') bs@(_:bs') =
    longest
        (lcsa as bs')
        (lcsa as' bs)

longest :: forall a. List a -> List a -> List a
longest a b = if L.length a > L.length b
              then a
              else b
