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
  , "node-child-process"
  , "psci-support"
  , "string-parsers"
  , "stringutils"
  , "sunde"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
