module DiffMatchPatch where

import Prelude (map, ($), (<<<), (<), (>))
import Types

import Data.Tuple (Tuple(..))
import Data.Tuple.Native (T2, xt)

type DiffConfig = { editCost :: Int }
foreign import data DiffMatchPatch :: Type
foreign import diffMatchPatch :: DiffConfig -> DiffMatchPatch
foreign import diffImpl :: DiffMatchPatch -> String -> String -> Array (T2 Int String)

dmpDefaults :: DiffConfig
dmpDefaults = { editCost: 4 }

diff :: DiffMatchPatch -> String -> String -> Array Segment
diff dmp a b = map (toSegment <<< xt) $ diffImpl dmp a b
    where
        toSegment (Tuple p s) | p > 0 = Plus s
        toSegment (Tuple m s) | m < 0 = Minus s
        toSegment (Tuple _ s) = Same s
