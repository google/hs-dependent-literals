# dependent-literals

Pseudo-dependently-typed integral literals for Haskell.

[![Stack CI](https://github.com/google/hs-dependent-literals/actions/workflows/stack-ci.yml/badge.svg)](https://github.com/google/hs-dependent-literals/actions/workflows/stack-ci.yml)

## Disclaimer

This is not an officially supported Google product.

## Hackage Status

* [![numeric-kinds](https://badgen.net/runkit/awpr/hackage/v/numeric-kinds?icon=haskell&cache=600)](https://hackage.haskell.org/package/numeric-kinds)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/numeric-kinds?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/numeric-kinds?cache=600)
* [![snumber](https://badgen.net/runkit/awpr/hackage/v/snumber?icon=haskell&cache=600)](https://hackage.haskell.org/package/snumber)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/snumber?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/snumber?cache=600)
* [![dependent-literals](https://badgen.net/runkit/awpr/hackage/v/dependent-literals?icon=haskell&cache=600)](https://hackage.haskell.org/package/dependent-literals)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/dependent-literals?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/dependent-literals?cache=600)
* [![dependent-literals-plugin](https://badgen.net/runkit/awpr/hackage/v/dependent-literals-plugin?icon=haskell&cache=600)](https://hackage.haskell.org/package/dependent-literals-plugin)
  ![Uploaded](https://badgen.net/runkit/awpr/hackage/t/dependent-literals-plugin?cache=600)
  ![Haddock](https://badgen.net/runkit/awpr/hackage/d/dependent-literals-plugin?cache=600)

## Overview

This repository contains several packages implementing pseudo-dependently-typed
integral literals in GHC Haskell.

In standard Haskell, integer literals `42` are interpreted under the hood as a
call to the `Num` method `fromInteger 42 :: Num a => a`.  This means the type is
determined solely by the context, and any failures of bounds checking can be
observed only at runtime.  This plugin replaces that mechanism with one that
reflects the literal's value to the type level, and allows instances to refine
the resulting type and perform type-level bounds checks according to the value
of the literal.

For example, given `mkVec :: SInt n -> (Fin n -> a) -> Vec n a`, `mkVec 4` will
have type `(Fin 4 -> a) -> Vec 4 a`, and trying to type-check `4 :: Fin 4` will
report a type error saying that `4` is out of range.

Patterns get a similar treatment, too, so `case (x :: SInt n) of { 1 -> Just
Refl; _ -> Nothing } :: Maybe (n :~: 1)` can type-check: the act of matching
against 1 proved that the type-level `n` was equal to 1.
