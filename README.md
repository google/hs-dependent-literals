# dependent-literals

Pseudo-dependently-type integral literals for Haskell.

## Disclaimer

This is not an officially supported Google product.

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
