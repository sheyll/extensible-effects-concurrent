See also http://pvp.haskell.org/faq

## 0.94.0.0

- **Breaking change**: Switch RegExp API from the previously used `Monad(fail)` to `MonadFail(fail)` to denote matching failures

- Define `Extract Text` instances for strict and lazy `Text` types

- Compatibility with `base-4.13.0`

- Explicitly declare all modules `Safe` under SafeHaskell for GHC 7.4 and higher

----
