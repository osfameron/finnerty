module Source where

import Prelude

import Data.Either (Either)
import Data.List (List, fromFoldable, (:))
import Data.Traversable (scanl)

import Data.String (length)
import Data.String.Regex (Regex, regex, split)
import Data.String.Regex.Flags (global, multiline)

import Effect (Effect)
import Effect.Console (logShow)

example :: String
example = "module\n\nimport\nimport\nimport\n\ndata Foo =\n   Foo a\n   |Baz b\n\nfoo :: Foo -> Int\nfoo = identity"

compile :: String -> Either String Regex
compile re = regex re (global <> multiline)

regexIndices :: String -> String -> Either String (List Int)
regexIndices re str = do
    rx <- compile re
    strings <- pure $ fromFoldable $ split rx str
    pure $ 0 : scanl (\b a -> b + length a) 0 strings

indices :: String -> Either String (List Int)
indices = regexIndices "(?<=\\n\\n)(?=\\S)"

main :: Effect Unit
main = do
    logShow $ indices example
