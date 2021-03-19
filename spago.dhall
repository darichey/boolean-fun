{ name = "boolean-fun"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "psci-support"
  , "spec"
  , "spec-discovery"
  , "string-parsers"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
