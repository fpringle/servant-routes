# Changelog

All notable changes to `servant-routes` will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Haskell Package Versioning Policy](https://pvp.haskell.org).

## [Unreleased]

## [0.1.1.0] - 10.06.2025

### Added

- A function to sort `Route`s before printing them to stdout. [#32](https://github.com/fpringle/servant-routes/pull/32)
- More GHC versions in `tested-with`.
- Add support for Servant's `Description` and `Summary` combinators. [#29](https://github.com/fpringle/servant-routes/pull/29)

### Fixed

- Bug in the hand-rolled `Ord Param` instance.

## [0.1.0.0] - 03.05.2025

### Added

- First edition of the package.
- 100% documentation coverage.
- Almost 100% test coverage.
- Reasonably detailed README.
- CI that builds and tests the package for each version of GHC in the `tested-with` field.

[unreleased]: https://github.com/fpringle/servant-routes/compare/v0.1.1.0...HEAD
[0.1.1.0]: https://github.com/fpringle/servant-routes/releases/tag/v0.1.1.0
[0.1.0.0]: https://github.com/fpringle/servant-routes/releases/tag/v0.1.0.0
