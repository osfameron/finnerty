module Main where

import Prelude
import Node.ChildProcess as CP
import Sunde as S
import Data.Maybe (Maybe(..), fromJust)

import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console

import Effect (Effect)
import Effect.Console (log)

-- import Text.Parsing.StringParser.Combinators (many)
import Data.Array (many)
import Data.String.CodeUnits (fromCharArray)
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, runParser, try)
import Text.Parsing.StringParser.Combinators (optional, sepEndBy, manyTill, (<?>))
import Text.Parsing.StringParser.CodeUnits (string, eof, char, anyChar, regex)

import Data.Int as I
import Partial.Unsafe (unsafePartial)

import Data.Generic.Rep
import Data.Generic.Rep.Show

main :: Effect Unit
main = launchAff_ do
          result <- S.spawn { args: [], cmd: "ls", stdin: Nothing }  CP.defaultSpawnOptions
          Console.log result.stdout

data Segment = Same String
          | Plus String
          | Minus String
derive instance genericSegment :: Generic Segment _
instance showSegment :: Show Segment where
  show = genericShow

data Line = Insert String
          | Delete String
          | Modify (Array Segment)
derive instance genericLine :: Generic Line _
instance showLine :: Show Line where
  show = genericShow

segment :: Parser Segment
segment = segment' " " Same
      <|> segment' "+" Plus
      <|> segment' "-" Minus

segment' :: String -> (String -> Segment) -> Parser Segment
segment' s a = do
  _ <- string s
  s <- regex "[^\n]+"
  pure $ a s

segmentNL = do
  s <- segment
  _ <- nl
  pure s

nl :: Parser Unit
nl = void $ char '\n'

newSegment :: Parser Unit
newSegment = void $ string "~\n"

toInt :: String -> Int
toInt s = unsafePartial $ fromJust $ I.fromString s

int :: Parser Int
int = toInt <$> regex "[1-9][0-9]*"
        <?> "Not an integer"

intpair = int2 <|> int1

int1 = do
  s <- int
  pure $ { start: s, count: 1 }

int2 = try do
  s <- int
  _ <- string ","
  c <- int
  pure $ { start: s, count: c }


line = do
  ss <- many segmentNL
  case ss of
    [Plus s] -> pure $ Insert s
    [] -> pure $ Insert ""
    [Minus s] -> pure $ Delete s
    otherwise -> pure $ Modify ss

whole = sepEndBy line newSegment

main2 = runParser whole src

src = """ Segment' s a = do
~
-_ <- string s
~
 _ <- 
-eof
+optional nl
~
~
+pure $ a l
~"""
