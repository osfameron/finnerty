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
    let empty = [] :: Array Int

    test "empty" do
        Assert.equal empty (lcs empty empty)
    test "single" do
        Assert.equal [1] (lcs [1] [1])
    test "no match" do
        Assert.equal empty (lcs empty [1])
        Assert.equal empty (lcs [1] empty)
    test "one match at start" do
        Assert.equal [1] (lcs [1] [1,2])
        Assert.equal [1] (lcs [1,2] [1])
        Assert.equal [1] (lcs [1,2] [1,3])
        Assert.equal [1] (lcs [1,3] [1,2])
    test "two match at start" do
        Assert.equal [1,2] (lcs [1,2] [1,2,3])
        Assert.equal [1,2] (lcs [1,2,3] [1,2])
        Assert.equal [1,2] (lcs [1,2,3] [1,2])
    test "match at middle" do
        Assert.equal [2] (lcs [1,2,4] [2,3])
        Assert.equal [2,3] (lcs [1,2,3,4] [2,3])
        Assert.equal [2,3] (lcs [1,2,3,4] [0,2,3,5])
    test "match interspersed" do
        Assert.equal [2,3,5] (lcs [2,3,4,5] [1,2,3,3,5,6])





