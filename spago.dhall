{ name = "boolean-fun"
, dependencies =
  [ "console", "effect", "halogen", "psci-support", "string-parsers" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
