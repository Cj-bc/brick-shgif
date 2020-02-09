# Changelog for brick-shgif
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Haskell Package Versioning policy](https://pvp.haskell.org/).

## Unreleased changes

## [1.1.4.3] - 2020-02-09

### Added
- New shgif viewer executable with '--reversed'/'--oneshot' flags(see its '--help')

### Changed
- All example codes are moved to [example/codes/](example/codes/) directory.
- Update gitignore file to ignore files for 'vim'/'macOS'/'haskell'

### Removed
- Unused src/Lib.hs


## [1.1.4.2] - 2020-02-04

### Fixed
- 'updateShgifTo' used wrong operator which seems to be reason of the freeze


## [1.1.4.1] - 2020-02-04

### Changed
- 'addInitialCanvas' now returns Shgif with canvas rendered
- You don't have to call 'updateShgif' just after 'getShgif' to render it.

### Fixed
- Example code in README

## [1.1.4.0] - 2020-02-04

### Added
- 'updateShgifTo' that will update Shgif to make it closer to given tick

### Fixed
- Refactoring 'updateShgif' related functions with Lens

## [1.1.3.1] - 2019-12-22

### Fixed
- Freezing issue after 'updateShgifReversedNoLoop' is called with tick 0

## [1.1.3.0] - 2019-12-22

### Added
- Export 'Shgif.Type.updateShgifReversedNoLoop'

## [1.1.2.0] - 2019-12-22

### Added
- 'updateShgifReversedNoLoop' that is no-loop version of 'updateShgifReversed'

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
