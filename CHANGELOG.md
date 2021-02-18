# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v5.0.0](https://github.com/purescript/purescript-parallel/releases/tag/v5.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#37)

New features:
- Added instances for `Star` and `Costar` (#28)

Bugfixes:
- Fixed documentation typo: arguement -> argument (#33)

Other improvements:
- Migrated CI to GitHub Actions and updated installation instructions to use Spago (#36)
- Replaced dead URI in tests (#32)
- Added a changelog and pull request template (#37)

## [v4.0.0](https://github.com/purescript/purescript-parallel/releases/tag/v4.0.0) - 2018-05-23

- Updated for PureScript 0.12

## [v3.3.1](https://github.com/purescript/purescript-parallel/releases/tag/v3.3.1) - 2017-10-23

- Removed self-import in `Class` module (@paulyoung)

## [v3.3.0](https://github.com/purescript/purescript-parallel/releases/tag/v3.3.0) - 2017-09-10

- Added `parApply` function (@LukaJCB)

## [v3.2.0](https://github.com/purescript/purescript-parallel/releases/tag/v3.2.0) - 2017-08-19

- Added `parOneOf` and `parOneOfMap` (@natefaubion)

## [v3.1.0](https://github.com/purescript/purescript-parallel/releases/tag/v3.1.0) - 2017-08-18

- Added `parOneOf` and `parOneOfMap` (@natefaubion)

## [v3.0.0](https://github.com/purescript/purescript-parallel/releases/tag/v3.0.0) - 2017-03-27

- Updated for PureScript 0.11

## [v2.1.0](https://github.com/purescript/purescript-parallel/releases/tag/v2.1.0) - 2016-11-01

- Added `parSequence` and `parSequence_`

## [v2.0.0](https://github.com/purescript/purescript-parallel/releases/tag/v2.0.0) - 2016-10-20

- Updated dependencies
- New formulation based on functional dependencies (see #10)

## [v1.1.0](https://github.com/purescript/purescript-parallel/releases/tag/v1.1.0) - 2016-08-16

- Added `MonadRace` and `MonadPar` instances for `MaybeT` and `WriterT`

## [v1.0.0](https://github.com/purescript/purescript-parallel/releases/tag/v1.0.0) - 2016-06-06

- Updated for PureScript 0.9.1. **Note**: The v1.0.0 tag is not meant to indicate the library is “finished”, the core libraries are all being bumped to this for the 0.9 compiler release so as to use semver more correctly.

## [v0.5.1](https://github.com/purescript/purescript-parallel/releases/tag/v0.5.1) - 2015-09-19

- Exported `race` and `par` combinators (@paf31)

## [v0.5.0](https://github.com/purescript/purescript-parallel/releases/tag/v0.5.0) - 2015-09-16

- Bumped dependencies

## [v0.4.0](https://github.com/purescript/purescript-parallel/releases/tag/v0.4.0) - 2015-08-25

- Bumped `transformers` dependency to `0.7.1`. As such, this release requires version `0.7.4` of the PureScript compiler.

## [v0.3.0](https://github.com/purescript/purescript-parallel/releases/tag/v0.3.0) - 2015-06-30

- Updated for PureScript 0.7.\*. It will not work with older versions. If you are using an older version, you should require an older, compatible version of this library.

## [v0.2.1](https://github.com/purescript/purescript-parallel/releases/tag/v0.2.1) - 2015-03-05

- Added some convenience functions.

## [v0.2.0](https://github.com/purescript/purescript-parallel/releases/tag/v0.2.0) - 2015-03-05

- Hid `Parallel` constructor and added docs.

## [v0.1.0](https://github.com/purescript/purescript-parallel/releases/tag/v0.1.0) - 2014-11-26

- Initial release
