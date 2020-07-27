module Zipper where

import Prelude
import Types

import Data.Traversable (scanl)
import Data.List (List(..), (:), reverse, filter, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S

import Control.Monad.Loops (iterateUntilM)

import Effect (Effect)
import Effect.Console as Console

import Data.Generic.Rep (class Generic)
import Data.Debug (class Debug, genericDebug, prettyPrintWith, debug)

-- | `Item` is a pair of a Value and an Accumulator.
-- | For example `Item String Int` might represent a string and its position in a document.
type Item a b = {val :: a, acc :: b}
item :: forall a b. a -> b -> Item a b
item a b = {val: a, acc: b}

-- | # Zipper
-- | This is a simple zipper over a list, with an accumulator
-- |   `Zipper Crumb Next Zero Acc`
-- | Crumb: list of current and previous items (the breadcrumb trail), e.g. values and accumulators.
-- | Next: list of future values
-- | Zero: A Zero item, used for a Nil list.
-- | Acc: A function which takes an item (value + accumulator) and returns the next accumulator
data Zipper a b = Zipper (List (Item a b)) (List a) (Item a b) (Item a b -> b)
derive instance genericZipper :: Generic (Zipper a b) _
instance debugZipper :: (Debug a, Debug b) => Debug (Zipper a b) where
  debug = genericDebug

-- | accessor for the crumb (current and previous items)
_crumb :: forall a b. Zipper a b -> List (Item a b)
_crumb (Zipper c _ _ _) = c

-- | convenience function to create a Zipper.
-- |   `zipper values zero acc`
zipper :: forall a b. (List a) -> (Item a b) -> (Item a b -> b) -> Zipper a b
zipper Nil z f = Zipper Nil Nil z f -- we only need z for this case, bah
zipper (h:t) z f = Zipper (z {val=h} : Nil) t z f

-- | move to next position in Zipper.
-- | Returns a Maybe, e.g. Nothing if at end of zipper.
next :: forall a b. Zipper a b -> Maybe (Zipper a b)
next (Zipper _ Nil _ _) = Nothing
next (Zipper Nil (new:rest) z f) = Just $ Zipper (z {val=new} : Nil) rest z f
next (Zipper prv@(curr:_) (new:rest) z f) =
    let
        focus = item new (f curr)
        prv' = (focus: prv)
    in Just $ Zipper prv' rest z f

_iterateTill :: forall a b.
    (Zipper a b -> Maybe (Zipper a b))
    -> (Item a b -> Boolean)
    -> Zipper a b
    -> Maybe (Zipper a b)
_iterateTill transform pred zipper
    = iterateUntilM pred' transform zipper
    where
        pred' z' = fromMaybe false $ do
            h <- head $ _crumb z'
            pure $ pred h

-- | carry on moving `next` in a Zipper till predicate holds true
nextTill :: forall a b. (Item a b -> Boolean) -> Zipper a b -> Maybe (Zipper a b)
nextTill = _iterateTill next

-- | move back in a zipper, returning `Nothing` if already at beginning
prev :: forall a b. Zipper a b -> Maybe (Zipper a b)
prev (Zipper Nil _ _ _) = Nothing
prev (Zipper (curr:rest) nxt z f) = Just $ Zipper rest (_.val curr: nxt) z f

-- | carry on moving `prev` in a Zipper till predicate holds true
prevTill :: forall a b. (Item a b -> Boolean) -> Zipper a b -> Maybe (Zipper a b)
prevTill = _iterateTill prev

-- | come out of the zipper, returning a list of values
unZip :: forall a b. Zipper a b -> List a
unZip (Zipper prv succ _ _) =
    (reverse $ map _.val prv)
    <>
    succ

-- | update value at the current location by passing in a function from val -> val
update :: forall a b. (a -> a) -> Zipper a b -> Zipper a b
update _ z@(Zipper Nil _ _ _) = z
update t (Zipper ({val,acc}:prv) rest z f)
    = Zipper (item (t val) acc: prv) rest z f

-- | split current Item. Pass in a function which returns a list of values.
-- | You could return e.g.
-- |  * zero values: (deleting this item. But see `delete`)
-- |  * 1 value: changed or unchanged
-- |  * 2+ values: splitting the insertion point
-- | The cursor will be left at the first value (or the previous one in the case of a deletion)
split :: forall a b. (Item a b -> List a) -> Zipper a b -> Zipper a b
split _ z@(Zipper Nil _ _ _) = z
split t (Zipper (h:prv) rest z f) =
    let vals = t h
        aux :: Item a b -> a -> Item a b
        aux i new = item new (f i)
        items = scanl aux z{acc=h.acc} vals
    in Zipper (reverse items <> prv) rest z f

-- | Delete the node at the cursor
delete :: forall a b. Zipper a b -> Zipper a b
delete = split (const Nil)

-- | # Functions operating on Zipper Segment Int
type SegmentAcc = {old :: Int, new :: Int}

zero :: Item Segment SegmentAcc
zero = {val: Same "", acc: {old: 0, new: 0}}

segInc :: Item Segment SegmentAcc -> SegmentAcc
segInc {acc: acc@{old, new}, val} =
    case val of
        Plus s -> acc { new = new + S.length s }
        Minus s -> acc { old = old + S.length s }
        Same s ->
            let len = S.length s
            in {old: old + len, new: new + len}

segZipper :: (List Segment) -> Zipper Segment SegmentAcc
segZipper segments = zipper segments zero segInc

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


-- | A debug instance which shows enough context to be useful for zippers.
-- | At repl, call `:print Zipper.myDebug`
myDebug :: forall d. Debug d => d -> Effect Unit
myDebug = Console.log <<< prettyPrintWith {compactThreshold: 6, maxDepth: Just 6} <<< debug



