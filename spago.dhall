{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "ansi"
  , "console"
  , "debugged"
  , "effect"
  , "generics-rep"
  , "jack"
  , "monad-loops"
  , "node-child-process"
  , "psci-support"
  , "string-parsers"
  , "stringutils"
  , "sunde"
  , "tuples-native"
  , "typelevel"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
