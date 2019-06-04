
# Ideas for future Versions

- Put timing into pure processes!
- Every `Api` type instance now **must** be an `NFData` 
  and a `ToLogText` instance
- `call` will always require a `Timeout`
- `call` now monitors the caller
- `call` now monitors the called process
- `call` now returns `Either TimeoutError a`   

- Logging improvements:
    - Introduce `ToLogText`
    - Remove `ToLogMessage`
    - Remove `logXXX'` users have to use `logXXX` and `ToLogText` 

- Move the `Supervisor` child lookup table into an `MVar`
