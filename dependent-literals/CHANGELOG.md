# 0.2.0

* Update for `numeric-kinds-0.2.0`.
  * Check inequality tests like `<?` against `True` rather than checking
    `Ordering`s against `EQ`.
  * This could in theory require updating constraints, but probably won't break
    anything in practice.

# 0.1.1.0 (2021-09-09)

* Moved the `StockLit` instance to `Wrapped Num` (kept `StockLit` as a newtype).

# 0.1.0.0 (2021-09-08)

Initial version.
