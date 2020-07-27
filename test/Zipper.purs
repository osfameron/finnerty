module Test.Zipper where

import Prelude

import Zipper
import Types

import Jack
import Data.List
import Data.String.CodeUnits


prop_roundTrip :: Property
prop_roundTrip =
    forAll (listOf genSegment) \segs ->
    let zipper = segZipper segs
        segs' = unZip zipper
    in property $ segs == segs'

x = _.foo

genZipper :: Gen (Zipper Segment SegmentAcc)
genZipper = listOf genSegment >>= segZipper >>> pure

genSegment :: Gen Segment
genSegment =
    oneOf
        [ Plus <$> genString
        , Minus <$> genString
        , Same <$> genString ]

genString :: Gen String
genString =
  map fromCharArray $ arrayOf $ chooseChar 'a' 'd'
