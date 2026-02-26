# Revelation-H (ALPHA ALPHA ALPHA!)
The password manager

This is a Haskell port of the program https://revelation.olasagasti.info/ (https://github.com/mikelolasagasti/revelation)

The project consists of several subprojects in correspond directories:

* lib-revelation-h -- the library to decode and encode Revelation's files
* lib-revelation-h-example -- the example do demonstrate how to decode and encode
* revelation-h -- GTK4 viewer of Revelation's files.

## Build

To build, you have to have Nix package manager installed: https://nixos.org/download/

1. Run `nix-shell` in the root project directory. It will prepare you environment: install Haskell, cabal and required libraries.
2. Run `cabal build <subproject>` or `cabal build all`.

## Running a project

Call the command `cabal run <subproject>`.
