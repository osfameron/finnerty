module Main where

import Prelude
import Node.ChildProcess as CP
import Sunde as S
import Data.Maybe (Maybe(..), fromJust)
import Effect.Aff (launchAff_, Aff)
import Effect.Exception (error)
import Effect.Class.Console as Console
import Effect (Effect)
import Control.Monad.Error.Class (throwError)
import Ansi.Codes
import Ansi.Output

-- import Text.Parsing.StringParser.Combinators (many)
import Data.Array (many, toUnfoldable)
import Data.Either (Either, fromRight)
import Data.Foldable (intercalate)
import Data.List (List(..), (:), reverse)
import Data.List.Types (NonEmptyList(..))
import Data.String.Utils (lines)
import Data.NonEmpty (head, tail)
import Data.Traversable (traverse)
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
main =
  let args = {commit: "b733e3ba660854386b481966948cc2dd61183f2c"
             ,file: "src/Main.purs" }
  in
    launchAff_ $ mainAff args

type Args = {commit :: String, file :: String}

mainAff :: Args -> Aff Unit
mainAff args =  do
  b <- baseline args
  d <- diff args
  let output = applyHunks b d
  void $ traverse Console.log $ formatOutput output
  Console.log $ show d

formatOutput :: List Output -> List String
formatOutput os = map formatOutput' os
  where formatOutput' (Context s) = s
        formatOutput' (Focus (Insert s)) = withGraphics (bold <> foreground BrightGreen) s
        formatOutput' (Focus (Delete s)) = withGraphics (bold <> foreground BrightRed) s
        formatOutput' (Focus (Modify ss)) = intercalate "***" $ map formatSegment ss
        formatSegment (Same s) = withGraphics bold s
        formatSegment (Plus s) = withGraphics (bold <> foreground BrightGreen) s
        formatSegment (Minus s) = withGraphics (bold <> foreground BrightRed) s




runCmd :: forall a. String -> Array String -> (String -> a) -> Aff a
runCmd cmd args f = do
  result <- S.spawn
    { cmd: cmd
    , args: args
    , stdin: Nothing }
    CP.defaultSpawnOptions
  case result.exit of
    CP.Normally 0 -> pure $ f result.stdout
    otherwise -> throwError $ error result.stderr

-- diff :: Args -> Aff DiffResult
diff args =
  runCmd
    "git"
    [ "diff"
    , "-U0"
    , "--function-context"
    , "--word-diff=porcelain"
    , "--word-diff-regex=\"(\\\\w+|.)\"]"
    , args.commit <> "^"
    , args.commit
    , args.file]
    (toUnfoldable <<< unsafePartial fromRight <<< runParser whole)

baseline :: Args -> Aff (List String)
baseline args =
  runCmd
    "git"
    [ "show"
    , args.commit <> "^:" <> args.file]
    (toUnfoldable <<< lines)

type DiffResult = Either ParseError (Array Hunk)
type Hunk = { header :: { from :: CountStart, to :: CountStart }, body :: List Line }
type CountStart = { count :: Int, start :: Int }

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
toInt s = unsafePartial $ fromJust $ I.fromString s

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

splitAt :: forall a. Int -> List a -> Tuple (List a) (List a)
splitAt = splitAt' Nil
  where splitAt' xs 0 ys = Tuple (reverse xs) ys
        splitAt' xs _ Nil = Tuple (reverse xs) Nil
        splitAt' xs n (y: ys) = splitAt' (y:xs) (n - 1) ys

data Output
  = Context String
  | Focus Line  

derive instance genericOutput :: Generic Output _

instance showOutput :: Show Output where
  show = genericShow

applyHunks :: List String -> List Hunk -> List Output
applyHunks = applyHunks' 1
  where
    applyHunks' _ xs Nil = map Context xs
    applyHunks' pos xs (h:hs) =
      let
        start = h.header.from.start
        count = h.header.from.count
        Tuple cs rest = splitAt (start - pos) xs
        Tuple _ rest' = splitAt count rest
      in
        map Context cs
        <> map Focus h.body
        <> applyHunks' (pos + start + count)  rest' hs
