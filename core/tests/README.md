# compilation tests

These tests have an odd structure.

First, they only need to be built – they do nothing at runtime.

Then there are a number of components:

- a separate `Test.Min_<x>_<y>_<z>` for each GHC version
- a `Test.Supported<language>` for some language editions
- a `test-suite` for each `default-language`

Each `Test.Min_<x>_<y>_<z>` enables all of the language extensions and warning flags[^1] that are supported by that compiler version, as well as a `-fplugin-opt GhcCompat:minVersion=x.y.z` option.

[^1]: Well, the warning flags that this plugin currently cares about.

When the tests are built with a particular GHC, each suite will build every `Test.Min__<x>_<y>_<z>` up to and including the one tagged with the current GHC version. This ensures we check every `minVersion` value against every GHC version.

The suite-per-`default-language` adds another dimension to the matrix, since the language edition implicitly changes the set of extensions, and we need to make sure that is handled correctly.

The `Test.Supported<language>` modules are just a convenient shorthand to avoid duplicating a number of conditionals in multiple places.
