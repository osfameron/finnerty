module Zipper.String where

import Prelude
import Zipper
import Data.List
import Data.Maybe
import Data.String as S

-- | # Functions operating on Zipper String Int

-- | For a `Zipper String Int`, split the node at the provided absolute position.
-- | If the position is before the accumulator, or after accumulator+length, then
-- | the zipper is returned unchanged.
splitAt :: Int -> Zipper String Int -> Zipper String Int
splitAt pos = split aux
    where aux {acc, val} = let
            ss = S.splitAt (pos - acc) val
            nonempty = (_ > 0) <<< S.length
        in filter nonempty (ss.before: ss.after: Nil)

goto :: Int -> Zipper String Int -> Maybe (Zipper String Int)
goto _ (Zipper Nil _ _ _) = Nothing
goto pos z@(Zipper (i@{acc}:_) _ _ f) =
    let start = acc
        end =  f i
        t = case pos of
            p | p >= end -> nextTill (f >>> (_ > pos))
            p | p < start -> prevTill (_.acc >>> (_ <= pos))
            otherwise -> pure
        in t z

-- | # Sample code
zeroString :: Item String Int
zeroString = {acc: 0, val: ""}

idx :: Zipper String Int
idx = zipper ("foo": "bar": "baz" : "qqux" : Nil) zeroString (\{acc, val} -> acc + S.length val)

len4 :: forall b. Item String b -> Boolean
len4 i = S.length i.val == 4

next4 :: Maybe (Zipper String Int)
next4 = nextTill len4 idx
