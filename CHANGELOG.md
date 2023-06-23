# Changelog for `pandoc-symreg`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

- Added CI tests

## 0.2.1.2 - 2023-06-23

- Changed to new version of srtree-eqsat using a lightweight version of the simplification process

## 0.2.1.1 - 2023-05-26

- Changed to optparse-applicative-0.18.0.0

## 0.2.1.0 - 2023-05-20

- Changed to srtree-1.0.0.1
- Fixed not parsing white spaces between operators
- Simplification with Equality Saturation

## 0.1.1.0 - 2023-02-13

- Added support to GP-GOMEA
- Added support to PySR
- Added `-simplify` flag to apply `SRTree` simplification

## 0.1.0.1 - 2023-01-13

### Changed

- Updated the bounds for `base` to work with stackage nightly

## 0.1.0.0 - 2023-01-13

### Added

- First release
- Added support to TIR, HeuristicLab, Operon, and Bingo algorithms
- Added support to python, math, TikZ, and laTeX outputs
- Added support to replace numerical values with adjustable parameters variables
