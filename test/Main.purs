module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.Zipper

import Jack.Runner (jackMain)

main :: Effect Unit
main =
  jackMain
    [ "Test.Zipper"
    ]
