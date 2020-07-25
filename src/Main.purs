module Main where

import Prelude
import Git
import Types
import Zipper as Z
import DiffMatchPatch as DMP

-- Basic Data
import Data.Foldable (intercalate)
import Data.List (List(..), (:), reverse, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))


-- Effects
import Effect (Effect)
import Effect.Aff (launchAff_, Aff)
import Effect.Class.Console as Console
import Data.Debug (debug, prettyPrintWith, prettyPrint)

-- Console and Debug
import Ansi.Codes (Color(..))
import Ansi.Output (bold, dim, foreground, withGraphics)

main :: Effect Unit
main =
  let args = {commit: "3cfa381c9d1461cf0d05cc3fb982089f9a5399cf"
             ,file: "test/Example.test" }
  in
    launchAff_ $ mainAff2 args

mainAff :: Args -> Aff Unit
mainAff args =  do
  b <- baseline args
  d <- diff args
  let output = applyHunks b d
  void $ traverse Console.log $ formatOutput output
  Console.log $ prettyPrintWith  { maxDepth: Just 8, compactThreshold: 4 } (debug d)

mainAff2 args = do
  f1 <- fileAt (prev args) identity
  f2 <- fileAt args identity
  let d = DMP.diff (DMP.diffMatchPatch DMP.dmpDefaults) f1 f2
  let z = Z.segZipper (fromFoldable d) # Z.next >>= Z.next
  Console.log $ prettyPrintWith  { maxDepth: Just 8, compactThreshold: 4 } (debug z)

formatOutput :: List Output -> List String
formatOutput os = map formatOutput' os
  where formatOutput' (Context s) = withGraphics dim s
        formatOutput' (Focus (Insert s)) = withGraphics (bold <> foreground BrightGreen) s
        formatOutput' (Focus (Delete s)) = withGraphics (bold <> foreground BrightRed) s
        formatOutput' (Focus (Modify ss)) = intercalate "" $ map formatSegment ss
        formatSegment (Same s) = s
        formatSegment (Plus s) = withGraphics (bold <> foreground BrightGreen) s
        formatSegment (Minus s) = withGraphics (bold <> foreground BrightRed) s



splitAt :: forall a. Int -> List a -> Tuple (List a) (List a)
splitAt = splitAt' Nil
  where splitAt' xs 0 ys = Tuple (reverse xs) ys
        splitAt' xs _ Nil = Tuple (reverse xs) Nil
        splitAt' xs n (y: ys) = splitAt' (y:xs) (n - 1) ys

applyHunks :: List String -> List Hunk -> List Output
applyHunks = applyHunks' 1
  where
    applyHunks' _ xs Nil = map Context xs
    applyHunks' pos xs (h:hs) =
      let
        start = h.header.from.start
        count = h.header.from.count
        Tuple cs rest = splitAt (start - pos) xs
        Tuple _ rest' = splitAt count rest
      in
        map Context cs
        <> map Focus h.body
        <> applyHunks' (start + count)  rest' hs
