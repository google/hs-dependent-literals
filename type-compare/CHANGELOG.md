# 0.1.1 (2021-11-05)

* Adapt `SOrdering` from `numeric-kinds` into a backfill for `OrderingI`.
  * The type and its constructors have been renamed.
  * `EQI` now requires/proves nominal equality of its type parameters.

# 0.1.0 (2021-11-05)

Initial version, moved out of `numeric-kinds`.

* `Compare` type family (re-exported from `base` when available).
* Equality/inequality test operators.
* Equality/inequality constraint operators.
* `Proven` and `OrdCond` utility type families.
