module LCS where

import Prelude
import Data.List

lcs :: forall a. Eq a => List a -> List a -> List a
lcs Nil _ = Nil
lcs _ Nil = Nil
lcs (a:_) (b:_) | a == b = a : Nil
lcs _ _ = Nil
