# Changelog for brick-shgif
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Haskell Package Versioning policy](https://pvp.haskell.org/).

## Unreleased changes

## [1.1.1.0] - 2019-12-22

### Added
- 'updateShgifReversed' that reverse shgif(play backwards)
- Two examples for 'updateShgifReversed'

## [1.1.0.2] - 2019-12-22

### Added
- 'updateShgifNoLoop' that is no-loop version of 'updateShgif'

### Fixied
- Add missing 'added component' to Changelog (see above)

## [1.1.0.1] - 2019-12-22

### Changed
- Use [Haskell Package Versioning policy](https://pvp.haskell.org/) instead of semantic versioning

## [1.1.0] - 2019-12-20

### Added
- 'getShgif' -- Read Shgif from Yaml and generate Canvas for it
- 'getShgif' -- Read Shgif from more than one Yaml and generate Canvas for each of them
- New example to treat more than one Shgif

### Changed
- 'shgif' widget isn't applied borer
- Rename 'app/Main.hs' to 'app/shgifView.hs'

### Removed
- 'Shgif.Type' stop exposing 'addInitialCanvas'

## [1.0.0] - 2019-12-19

### Added
- Add basic functionality to render Shgif file
- First Shgif format
