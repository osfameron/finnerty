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
import Data.Either (Either, Either(..))
import Data.List (List(..), (:), reverse)
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
import Data.Tuple (Tuple(..))

main :: Effect Unit
main = do
    let
      commit = "b733e3ba660854386b481966948cc2dd61183f2c"
      file = "src/Main.purs"
    d <- launchAff_ $ diff commit file
    Console.log $ show d

-- diff :: String -> String -> Effect Unit
diff commit file =  do
  result <- S.spawn
    { cmd: "git"
    , args:
      ["diff"
      ,"-U0"
      ,"--function-context"
      ,"--word-diff=porcelain"
      ,"--word-diff-regex=\"(\\\\w+|.)\"]"
      ,commit
      ,file]
    , stdin: Nothing }
    CP.defaultSpawnOptions
  let
    d = case result.exit of
      CP.Normally 0 -> Right $ runParser whole result.stdout
      otherwise -> Left result.stderr
  _ <- Console.log $ show d
  pure d

data Segment
  = Same String
  | Plus String
  | Minus String

derive instance genericSegment :: Generic Segment _

instance showSegment :: Show Segment where
  show = genericShow

data Line
  = Insert String
  | Delete String
  | Modify (Array Segment)

derive instance genericLine :: Generic Line _

instance showLine :: Show Line where
  show = genericShow

segment :: Parser Segment
segment =
  segment' " " Same
    <|> segment' "+" Plus
    <|> segment' "-" Minus

segment' :: String -> (String -> Segment) -> Parser Segment
segment' prefix a = do
  skipString prefix
  s <- regex "[^\n]+"
  skipNL
  pure $ a s

skip :: forall a. Parser a -> Parser Unit
skip = void

skipNL :: Parser Unit
skipNL = skip $ char '\n'

skipString :: String -> Parser Unit
skipString = skip <<< string

skipRegex :: String -> Parser Unit
skipRegex = skip <<< regex


newSegment :: Parser Unit
newSegment = skip $ string "~\n"

type CountStart
  = { count :: Int, start :: Int }

hunkHeader :: Parser { from :: CountStart, to :: CountStart }
hunkHeader = do
  skipString "@@ -"
  f <- intpair
  skipString " +"
  t <- intpair
  skipString " @@"
  skipRegex ".*" -- comments
  skipNL
  pure { from: f, to: t }

toInt :: String -> Int
toInt s = unsafePartial $ fromJust $ I.fromString s

int :: Parser Int
int =
  toInt <$> regex "[1-9][0-9]*"
    <?> "Not an integer"

intpair :: Parser { count :: Int, start :: Int }
intpair = do
  (NonEmptyList ints) <- sepBy1 int (char ',')
  let
    s = head ints
  let
    c = case tail ints of
      Nil -> 1
      (c' : _) -> c'
  pure { start: s, count: c }

line :: Parser Line
line = do
  ss <- many segment
  case ss of
    [ Plus s ] -> pure $ Insert s
    [] -> pure $ Insert ""
    [ Minus s ] -> pure $ Delete s
    otherwise -> pure $ Modify ss

hunkBody :: Parser (List Line)
hunkBody = sepEndBy line newSegment

type Hunk = { header :: { from :: CountStart, to :: CountStart }, body :: List Line }

hunk :: Parser Hunk
hunk = do
  h <- hunkHeader
  b <- hunkBody
  pure { header: h, body: b }

diffHeader = do
  skipRegex "diff .*\n"
  skipRegex "index .*\n"
  skipRegex "--- .*\n"
  skipRegex "[+]{3} .*\n"
  pure unit

whole = do
  skip $ diffHeader
  h <- many hunk
  pure h

-- testParse :: Either ParseError (List Line)
testParse = runParser whole src

splitAt :: forall a. Int -> List a -> Tuple (List a) (List a)
splitAt = splitAt' Nil
  where splitAt' xs 0 ys = Tuple (reverse xs) ys
        splitAt' xs _ Nil = Tuple (reverse xs) Nil
        splitAt' xs n (y: ys) = splitAt' (y:xs) (n - 1) ys

data Output
  = Context String
  | Focus Line  

applyHunks :: forall a b. List String -> List Hunk -> List Output
-- applyHunks src hunks
applyHunks = applyHunks' 1
  where
    applyHunks' _ xs Nil = map Context xs
    applyHunks' pos xs (h:hs) =
      let
        start = h.header.from.start
        count = h.header.from.count
        Tuple cs rest = splitAt (start - pos) xs
        Tuple _ rest' = splitAt count rest
      in (map Context cs) <> (map Focus h.body) <> applyHunks' (pos + start + count)  rest' hs

src :: String
src =
  """diff --git a/src/Main.purs b/src/Main.purs
index 3029fa9..632924f 100644
--- a/src/Main.purs
+++ b/src/Main.purs
@@ -4,12 +4,9 @@ hunk the first
 Same
~
-Delete
+Add
 Same
~
+Add
 Same
-Delete
~
@@ -10,22 +14,19 @@ another later hunk
 Same
~
~
-Delete
~
+Add
~
-Delete
+Add
 Same
~
 END
~"""
