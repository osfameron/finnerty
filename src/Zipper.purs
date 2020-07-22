module Zipper where

import Prelude

import Data.Traversable (scanl)
import Data.List (List(..), (:), reverse, filter, head)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S

import Control.Monad.Loops (iterateUntilM)

import Effect (Effect)
import Effect.Console as Console

import Data.Generic.Rep (class Generic)
import Data.Debug (class Debug, genericDebug, prettyPrintWith, debug)

type Item a b = {val :: a, acc :: b}
item :: forall a b. a -> b -> Item a b
item a b = {val: a, acc: b}

data Zipper a b = Zipper (List (Item a b)) (List a) (Item a b) (Item a b -> b)
derive instance genericZipper :: Generic (Zipper a b) _
instance debugZipper :: (Debug a, Debug b) => Debug (Zipper a b) where
  debug = genericDebug

zipper :: forall a b. (List a) -> (Item a b) -> (Item a b -> b) -> Zipper a b
zipper Nil z f = Zipper Nil Nil z f -- we only need z for this case, bah
zipper (h:t) z f = Zipper (z {val=h} : Nil) t z f

next :: forall a b. Zipper a b -> Maybe (Zipper a b)
next (Zipper _ Nil _ _) = Nothing
next (Zipper Nil (new:rest) z f) = Just $ Zipper (z {val=new} : Nil) rest z f
next (Zipper prv@(curr:_) (new:rest) z f) =
    let
        focus = item new (f curr)
        prv' = (focus: prv)
    in Just $ Zipper prv' rest z f

prev :: forall a b. Zipper a b -> Maybe (Zipper a b)
prev (Zipper Nil _ _ _) = Nothing
prev (Zipper (curr:rest) nxt z f) = Just $ Zipper rest (_val curr: nxt) z f

_crumb :: forall a b. Zipper a b -> List (Item a b)
_crumb (Zipper c _ _ _) = c

update :: forall a b. (a -> a) -> Zipper a b -> Zipper a b
update _ z@(Zipper Nil _ _ _) = z
update t (Zipper ({val,acc}:prev) rest z f)
    = Zipper (item (t val) acc: prev) rest z f

split :: forall a b. (Item a b -> List a) -> Zipper a b -> Zipper a b
split _ z@(Zipper Nil _ _ _) = z
split t (Zipper (h:prev) rest z f) =
    let vals = t h
        aux :: Item a b -> a -> Item a b
        aux i new = item new (f i)
        items = scanl aux z{acc=h.acc} vals
    in Zipper (reverse items <> prev) rest z f

delete :: forall a b. Zipper a b -> Zipper a b
delete = split (const Nil)

splitAt :: Int -> Zipper String Int -> Zipper String Int
splitAt pos = split aux
    where aux {acc, val} = let
            ss = S.splitAt (pos - acc) val
            nonempty = (_ > 0) <<< S.length
        in filter nonempty (ss.before: ss.after: Nil)


-- :print Zipper.myDebug
myDebug :: forall d. Debug d => d -> Effect Unit
myDebug = Console.log <<< prettyPrintWith {compactThreshold: 6, maxDepth: Just 6} <<< debug

zero :: Item String Int
zero = {acc: 0, val: ""}

idx :: Zipper String Int
idx = zipper ("foo": "bar": "baz" : "qqux" : Nil) zero (\{acc, val} -> acc + S.length val)

len4 :: forall b. Item String b -> Boolean
len4 i = S.length i.val == 4

next4 :: Maybe (Zipper String Int)
next4 = nextTill len4 idx

nextTill :: forall a b. (Item a b -> Boolean) -> Zipper a b -> Maybe (Zipper a b)
nextTill p z = iterateUntilM pred next z
    where
        pred z' = fromMaybe false $ do
            h <- head $ _crumb z'
            pure $ p h

_val i = i.val

unZip :: forall a b. Zipper a b -> List a
unZip (Zipper prev succ _ _) =
    (reverse $ map _val prev)
    <>
    succ
