module Test.Main where

import Prelude

import Effect (Effect)

import Jack.Runner (jackMain)

main :: Effect Unit
main =
  jackMain
    [ "Test.Zipper"
    ]
