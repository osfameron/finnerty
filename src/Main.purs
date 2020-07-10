module Main where

import Prelude
import Node.ChildProcess as CP
import Sunde as S
import Data.Maybe (Maybe(..))

import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console

import Effect (Effect)
import Effect.Console (log)

-- import Text.Parsing.StringParser.Combinators (many)
import Data.Array (many)
import Data.String.CodeUnits (fromCharArray)
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, runParser)
import Text.Parsing.StringParser.CodeUnits (string, eof, anyChar)

import Data.Generic.Rep
import Data.Generic.Rep.Show

main :: Effect Unit
main = launchAff_ do
          result <- S.spawn { args: [], cmd: "ls", stdin: Nothing }  CP.defaultSpawnOptions
          Console.log result.stdout

data Line = Plus String
          | Minus String
          | NewLine
derive instance genericLine :: Generic Line _
instance showLine :: Show Line where
  show = genericShow

line :: Parser Line
line = plusOrMinus "+" Plus
       <|> plusOrMinus "-" Minus
       <|> newline

plusOrMinus :: String -> (String -> Line) -> Parser Line
plusOrMinus s a = do
  _ <- string s
  l <- many anyChar
  _ <- eof
  pure $ a (fromCharArray l)

newline :: Parser Line
newline = do
  _ <- string "~"
  _ <- eof
  pure $ NewLine

parseLine = runParser line