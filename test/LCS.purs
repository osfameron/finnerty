module Test.LCS where

import Prelude
import LCS

import Test.Unit (suite, test, timeout)
import Test.Unit.Main (runTest)
import Test.Unit.Assert as Assert
import Effect
import Data.List

main :: Effect Unit
main = runTest do
  suite "LCS" do
    let empty = Nil :: List Int
    let one = 1 : Nil

    test "empty" do
        Assert.equal empty (lcs empty empty)
    test "single" do
        Assert.equal one (lcs one one)
    test "no match" do
        Assert.equal empty (lcs empty one)
        Assert.equal empty (lcs one empty)


