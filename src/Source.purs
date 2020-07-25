module Source where

import Prelude

import Data.Either (Either)
import Data.List (List, fromFoldable, (:))
import Data.Traversable (scanl)

import Data.String (length)
import Data.String.Regex (Regex, regex, split)
import Data.String.Regex.Flags (multiline, noFlags)

import Effect (Effect)
import Effect.Console (logShow)

example :: String
example = "module\n\nimport\nimport\nimport\n\ndata Foo =\n   Foo a\n   |Baz b\n\nfoo :: Foo -> Int\nfoo = identity"

compile :: String -> Either String Regex
compile re = regex re multiline

regexIndices :: String -> String -> Either String (List Int)
regexIndices re str = do
    rx <- compile re
    strings <- pure $ fromFoldable $ split rx str
    pure $ 0 : scanl (\b a -> b + length a) 0 strings

-- | Hunks start with a line at Column 0 that follows an empty line.
-- |
-- | This supports:
-- |
-- |  * PureScript/Haskell/Python like indentation rules
-- |    (including multiple contiguous lines at col 0 at start, 
-- |    e.g. type signature)
-- |  * Blocks of imports (col 0) grouped together and separated by
-- |    an empty line
-- |  * Java/C/Perl/Ruby style with an ending `}` or `end` token, as
-- |    long as it isn't separated by an empty line
-- |  * empty lines in the middle of a function (as long as they're followed by
-- |    an indented line)
indices :: String -> Either String (List Int)
indices = regexIndices "(?<=\\n\\n)(?=\\S)"

main :: Effect Unit
main = do
    logShow $ indices example
