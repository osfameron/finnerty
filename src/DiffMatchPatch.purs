module DiffMatchPatch where

import Prelude (map, ($))
import Data.Tuple (Tuple(..))
import Data.Tuple.Native (T2, prj)
import Data.Typelevel.Num.Reps (d0, d1)

foreign import diffImpl :: String -> String -> Array (T2 Int String)

xt :: forall a b. T2 a b -> Tuple a b
xt t = Tuple (prj d0 t) (prj d1 t)

diff :: String -> String -> Array (Tuple Int String)
diff a b = map xt $ diffImpl a b
