# 0.2.0

* Rename `Cmp` to `Compare` and make it just a re-export on `base` >= 4.16.
* Re-export `OrdCond` and add a backfill on `base` < 4.16.
  * Backfill moved / renamed from `Kinds.Integer.CaseOrdering`.
* Make inequality definitions more consistent with those added to `base`.
  * Now all inequality constraints (rather than just some) are defined by
    comparing the corresponding inequality tests to `True`.
  * Now inequality tests are all done by `OrdCond`, instead of specific type
    families.  (This makes error messages containing irreducable inequalities
    marginally worse).

# 0.1.0.0 (2021-09-08)

Initial version.
