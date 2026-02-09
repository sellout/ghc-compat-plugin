# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog 1.1](https://keepachangelog.com/en/1.1.0/),
and this project adheres to the [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.1.0.0] – 2026-02-09

### Added

- tests (#6)
- support for reporting language editions that aren’t supported by `minVersion` (#7)

### Fixed

- don’t report extensions implied by language (#6)
- if `minVersion` is provided multiple times, the last one will be used, rather than the first (#6)

## [0.0.2.0] – 2026-02-03

### Fixed

- `NamedFieldPuns` had erroneously been reported as unsupported before GHC 9.4.1. Now it correctly only reports before 6.10.1 (#4)
- improved some documentation (#2)

## [0.0.1.0] - 2026-02-01

### Added

- initial release of this package

<!-- NB: The version on the left is the Haskell package version (PVP), the version on the right is the repo (tag) version (SemVer). Their only relationship is that a change of any severity on the left implies a change of at least that severity on the right. -->

[0.1.0.0]: https://github.com/sellout/ghc-compat-plugin/compare/v0.2.0...v1.0.0
[0.0.2.0]: https://github.com/sellout/ghc-compat-plugin/compare/v0.1.0...v0.2.0
[0.0.1.0]: https://github.com/sellout/ghc-compat-plugin/releases/tag/v0.1.0
