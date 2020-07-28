module Test.Zipper where

import Prelude

import Zipper
import Zipper as Z
import Types

import Jack
import Data.Array
import Data.List
import Data.NonEmpty as NE
import Data.String.CodeUnits


prop_roundTrip :: Property
prop_roundTrip =
    forAll (listOf genSegment) \segs ->
    let zipper = segZipper segs
        segs' = unZip zipper
    in property $ segs == segs'

prop_roundTrip2 :: Property
prop_roundTrip2 =
    forAll (listOf genSegment) \segs ->
    let zipper = segZipper segs
        segs' = zipper # next' # next' # prev' # unZip
    in property $ segs == segs'



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
  map (fromCharArray <<< NE.fromNonEmpty cons) $ arrayOf1 $ chooseChar 'a' 'd'
