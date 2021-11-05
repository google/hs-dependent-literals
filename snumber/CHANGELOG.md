# 0.3.0 (2021-11-05)

* Get type-level `Ord` from the new `type-compare` compatibility library.
* Get 'SOrdering` from `type-compare` under the new name `OrderingI`.

# 0.2.0 (2021-10-24)

* Downgrade the `unSNumber` field selector to be unidirectional.
  * It could previously be used with record update syntax to construct invalid
    `SNumbers` out of valid ones.
  * Migration: Replace record updates of `unSNumber` with `N#`.
* Add an `SN` pattern synonym for safely extracting an `SNumber` value without
  `MagicHash`.

# 0.1.0.0 (2021-09-08)

Initial version.
