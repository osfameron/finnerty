module Git where

import Prelude
import Types (Args, CountStart, Hunk, Line(..), Segment(..))

import Data.Either (Either, fromRight)
import Data.Int as I
import Data.Array (many, toUnfoldable)
import Data.List (List(..), (:))
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.NonEmpty (head, tail)
import Data.String.Utils (lines)

import Partial.Unsafe (unsafePartial)

import Effect.Aff (Aff, error, throwError)

-- String Parsing
import Control.Alt ((<|>))
import Text.Parsing.StringParser (Parser, runParser, ParseError)
import Text.Parsing.StringParser.Combinators (sepBy1, sepEndBy, (<?>))
import Text.Parsing.StringParser.CodeUnits (char, regex, string)

-- Child Processes
import Node.ChildProcess (Exit(..), defaultSpawnOptions)
import Sunde (spawn)

type DiffResult = Either ParseError (Array Hunk)
runCmd :: forall a. String -> Array String -> (String -> a) -> Aff a
runCmd cmd args f = do
  result <- spawn
    { cmd: cmd
    , args: args
    , stdin: Nothing }
    defaultSpawnOptions
  case result.exit of
    Normally 0 -> pure $ f result.stdout
    otherwise -> throwError $ error result.stderr

diff :: Args -> Aff (List Hunk)
diff args =
  runCmd
    "git"
    [ "diff"
    , "-U0"
    , "--function-context"
    , "--word-diff=porcelain"
    , "--word-diff-regex=(\\w+|.)"
    , args.commit <> "^"
    , args.commit
    , args.file]
    (toUnfoldable <<< unsafePartial fromRight <<< runParser whole)

baseline :: Args -> Aff (List String)
baseline args = fileAt (prev args) (toUnfoldable <<< lines)

prev :: Args -> Args
prev r@{commit} = r { commit = commit <> "^" }

fileAt :: forall a. Args -> (String -> a) -> Aff a
fileAt args =
    runCmd
        "git"
        [ "show"
        , args.commit <> ":" <> args.file ]

segment :: Parser Segment
segment =
  segment' " " Same
    <|> segment' "+" Plus
    <|> segment' "-" Minus

segment' :: String -> (String -> Segment) -> Parser Segment
segment' prefix a = do
  skipString prefix
  s <- regex ".*"
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
toInt = unsafePartial fromJust <<< I.fromString

int :: Parser Int
int =
  toInt <$> regex "[0-9]+"
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
  ss <- many segment <?> "No line"
  case ss of
    [ Plus s ] -> pure $ Insert s
    [] -> pure $ Insert ""
    [ Minus s ] -> pure $ Delete s
    otherwise -> pure $ Modify ss

hunkBody :: Parser (List Line)
hunkBody = sepEndBy line newSegment

hunk :: Parser Hunk
hunk = do
  h <- hunkHeader <?> "No hunkHeader"
  b <- hunkBody <?> "No hunkBody"
  pure { header: h, body: b }

diffHeader :: Parser Unit
diffHeader = do
  skipRegex "diff .*\n"
  skipRegex "index .*\n"
  skipRegex "--- .*\n"
  skipRegex "[+]{3} .*\n"
  pure unit

whole :: Parser (Array Hunk)
whole = do
  skip $ diffHeader
  h <- many hunk
  pure h
