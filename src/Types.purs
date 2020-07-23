module Types where

import Data.List (List)
import Data.Generic.Rep (class Generic)
import Data.Debug (class Debug, genericDebug)

type Args = {commit :: String, file :: String}

type Hunk = { header :: { from :: CountStart, to :: CountStart }, body :: List Line }
type CountStart = { count :: Int, start :: Int }

data Segment
  = Same String
  | Plus String
  | Minus String

derive instance genericSegment :: Generic Segment _

instance debugSegment :: Debug Segment where
  debug = genericDebug

data Line
  = Insert String
  | Delete String
  | Modify (Array Segment)

derive instance genericLine :: Generic Line _

instance debugLine :: Debug Line where
  debug = genericDebug

data Output
  = Context String
  | Focus Line  

derive instance genericOutput :: Generic Output _

instance debugOutput :: Debug Output where
  debug = genericDebug
