# HushGP
A PushGP implementation in Haskell.

Note: **This is an imcomplete library at this time. I am still developing it.**

## Overview

I am developing this library using:
- GHC 9.8.2
- Cabal 3.12.1.0

In order to run this library. There is some manual configuration needed. For an example, check
out `src/HushGP/Problems/IntegerRegression.hs`. This contains the parts needed for a full evolutionary run.

The user is expected to provide their own data, their own fitness function, and the set of instructions
they would like to use. This set can be found in the files of `src/HushGP/Instructions`. You can also
check the haddock documentation for lists of these as well.

# Building Instructions

In order to build Hush with hmatrix, `lapack` development libraries must be installed on your
system.

dnf: `sudo apt install lapack-devel`

Nix doesn't have this problem.

## Nix Users

This took my machine about 2 hours to build the environment after running `nix develop`.
