module DiffMatchPatch where

import Prelude (map, ($))
import Data.Tuple (Tuple(..))
import Data.Tuple.Native (T2, xt)
import Data.Typelevel.Num.Reps (d0, d1)

foreign import diffImpl :: String -> String -> Array (T2 Int String)

diff :: String -> String -> Array (Tuple Int String)
diff a b = map xt $ diffImpl a b
