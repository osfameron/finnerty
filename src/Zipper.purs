module Zipper where

import Prelude

import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String as S

import Control.Monad.Loops (iterateUntilM)

import Data.Generic.Rep (class Generic)
import Data.Debug (class Debug, genericDebug)

type Item a b = {item :: a, acc :: b}
item :: forall t1 t2.
  t1
  -> t2
     -> { acc :: t2
        , item :: t1
        }
item a b = {item: a, acc: b}

data Zipper a b = Zipper (List (Item a b)) (List a) b (b -> a -> b)
derive instance genericZipper :: Generic (Zipper a b) _
instance debugZipper :: (Debug a, Debug b) => Debug (Zipper a b) where
  debug = genericDebug

zipper :: forall a b. (List a) -> b -> (b -> a -> b) -> Zipper a b
zipper Nil z f = Zipper Nil Nil z f -- we only need z for this case, bah
zipper (h:t) z f = Zipper (item h z : Nil) t z f

next :: forall a b. Zipper a b -> Maybe (Zipper a b)
next (Zipper _ Nil _ _) = Nothing
next (Zipper Nil (new:rest) z f) = Just $ Zipper (item new z : Nil) rest z f
next (Zipper prev@(curr:_) (new:rest) z f) =
    let
        acc = f curr.acc new
        prev' = (item new acc: prev)
    in Just $ Zipper prev' rest z f

head :: forall a b. Zipper a b -> Maybe (Item a b)
head (Zipper Nil _ _ _) = Nothing
head (Zipper (h:_) _ _ _) = Just h

idx :: Zipper String Int
idx = zipper ("foo": "bar": "bazz" : "qqux" : Nil) 0 (\b a -> b + S.length a)

len4 :: forall b. Item String b -> Boolean
len4 i = S.length i.item == 4

next4 :: Maybe (Zipper String Int)
next4 = nextTill len4 idx

nextTill :: forall a b. (Item a b -> Boolean) -> Zipper a b -> Maybe (Zipper a b)
nextTill p z = iterateUntilM pred next z
    where
        pred z' = fromMaybe false $ do
            h <- head z'
            pure $ p h
