module Main where

import Prelude
import Node.ChildProcess as CP
import Sunde as S
import Data.Maybe (Maybe(..), fromJust)

import Effect.Aff (launchAff_)
import Effect.Class.Console as Console

import Effect (Effect)

-- import Text.Parsing.StringParser.Combinators (many)
import Data.Array (many)
import Data.Either (Either)
import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.NonEmpty (head, tail)
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, runParser, ParseError)
import Text.Parsing.StringParser.Combinators (sepBy1, sepEndBy, (<?>))
import Text.Parsing.StringParser.CodeUnits (char, regex, string)

import Data.Int as I
import Partial.Unsafe (unsafePartial)

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

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

segmentNL :: Parser Segment
segmentNL = do
  s <- segment
  _ <- nl
  pure s

nl :: Parser Unit
nl = void $ char '\n'

newSegment :: Parser Unit
newSegment = void $ string "~\n"

hunkHeader :: Parser
  { from :: { count :: Int
            , start :: Int
            }
  , to :: { count :: Int
          , start :: Int
          }
  }
hunkHeader = do
  _ <- string "@@ -"
  f <- intpair
  _ <- string " +"
  t <- intpair
  _ <- string " @@"
  _ <- regex ".*" -- comments
  pure {from: f, to: t}

toInt :: String -> Int
toInt s = unsafePartial $ fromJust $ I.fromString s

int :: Parser Int
int = toInt <$> regex "[1-9][0-9]*"
        <?> "Not an integer"

intpair :: Parser { count :: Int, start :: Int }
intpair = do
  (NonEmptyList ints) <- sepBy1 int (char ',')
  let s = head ints
  let c = case tail ints of
            Nil -> 1
            (c':_) -> c'
  pure {start: s, count: c}

line :: Parser Line
line = do
  ss <- many segmentNL
  case ss of
    [Plus s] -> pure $ Insert s
    [] -> pure $ Insert ""
    [Minus s] -> pure $ Delete s
    otherwise -> pure $ Modify ss

whole :: Parser (List Line)
whole = sepEndBy line newSegment

testParse :: Either ParseError (List Line)
testParse = runParser whole src

src :: String
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
