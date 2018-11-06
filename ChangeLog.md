# Changelog for extensible-effects-concurrent

## 0.11.0

- Change the return type of `spawnCallbackObserver` from
  `Bool` to `ApiServerCmd`

## 0.10.0

- Re-introduce a Logs Effect but keep the LogWriter Reader
- Get rid of the LogWriterProxy and the implicit argument
- Make logging stricter: require log message to be NFData instances

## 0.9.2

- Try to adapt the dependency versions to make hackage happy again

## 0.9.1

- Add smart constructors for `MessageSelector`
- Remove `ReceiveMessage` `Process` action
- Rename `ReceiveMessageSuchThat` to `ReceiveSelectedMessage`
- Improve some Show instances, e.g. ProcessId
- Rewrite Logging API:
  - Vastly simplified API

## 0.9.0

- Make `ForkIOScheduler` faster and more robust
- Add `ProcessExitReason`
- Add `ProcessState`
- Add `ShutdownRequest` type
- Rewrite logging to be a `Reader` of a `LogWriter`
- Remove pure logging, the `Logs...` constraint must be
  accompanied by `Lifted IO` (or `MonadIO`) in many log functions
  most prominently `logMsg`
- Add a `lmDistance` field in `LogMessage`
- Add `increaseLogMessageDistance` and `dropDistantLogMessages`
  using the new `lmDistance` field
- Add a newtype for the argument to selective receives: `MessageSelector`
- Add a `makeReference` function to `Process` which will return process local
  unique `Int`s
- Rename `spawnServer` to `spawnServerWithEffects` and add a simpler version of
  `spawnServerWithEffects` called `spawnServer`
- Make all `ApiHandler` handler callbacks optional (by changing the type to `Maybe ...`)
- `ApiHandler` must now return an `ApiServerCmd`.
- Add `ApiServerCmd` which allows handler functions to leave to server loop without
  exitting the process
- Fix `Observer.Queue`
- Rename fields in `ApiHandler`
- Add smart constructors for `ApiHandler`

## 0.8.0

- Add selective receive
- Complete `Api.Server` rewrite (simplification)
- Move examples to `./examples/` and add executables to the
  cabal file

## 0.7.3

- Add `withFrozenCallStack` to exposed functions
- Add `ObserverState` type alias

## 0.7.2

- Add `ObservationQueue` utility
- Fix missing re-exports from
  `Control.Eff.Concurrent.Api.Client`
  in `Control.Eff.Concurrent` introduced in recent versions

## 0.7.1

- Improve call-stack support in log messages
- Expose `setLogMessageTimestamp` and `setLogMessageThreadId`

## 0.7.0

- Remove the parameter from `closeLogChannelAfter` that had the optional:
  last-log-message-before-channel-closes

## 0.6.4

- Add `whereIsServer`

## 0.6.3

- Add `ServerReader` type alias

## 0.6.2

- Fix bad `containers` version boundary

## 0.6.1

- Improve Experimental Nix Expressions

## 0.6.0

- Rewrite Logging
- Improve Experimental Nix Expressions

## 0.5.0.0

- Switch to `extensible-effects` version `3.1.0.0`
- Bump to stackage LTS-12.9
- Add `Control.Eff.Log.MessageFactory`
- Add `Control.Eff.Log.Message`

## 0.4.0.0

- Switch to `extensible-effects` version `3.0.0.0`
- Improve single threaded scheduler to be more space efficient
- Add some strictness annotations
- Add `Control.Eff.Loop` with (hopefully) constant space `forever` and
  `replicateM_`
- Add `Control.Eff.Concurrent`, a module that conveniently re-exports most
  library functions.

## 0.3.0.2

- Improve single threaded scheduler such that the main process can return a value

## 0.3.0.1

- Fix a race condition in the SchedulerSession shutdown
- Improve the interactive scheduler session API
- Rename `SchedulerVar` -> `SchedulerSession`
- Remove `submitPrint`

## 0.3.0.0

- Add support for running and interacting with a scheduler
  and it's processes from IO, for example from ghci
- Rename `yieldProcess` to `executeAndResume`
- Add an actual `yieldProcess`, that behaves like `yield`
- Change the return type of function to `()` where applicable
  to avoid all these `_ <- sendMessage...` or `void $ sendMessage`
  dances.
- Add a simple logging observer: `spawnLoggingObserver`
- Removed `Control.Eff.Interactive`
- Removed most functions in `Control.Eff.ExceptionExtra`
- Make `sendMessage` and the reply in `call` strict with respect to the message
  payload

## 0.2.0.3

- Improve 'Api' documentation
- Improve `LogChannel` API
- Reorganize unit tests
- Hopefully tune travis ci test parameter enough to get a stable build result

## 0.2.0.2

- Fix minor `stack upload` complaints about the cabal file

## 0.2.0.1

- Simplify IO Exception handling in `ForkIoScheduler`,
- Add many unit tests for exception, exit and shutdown

## 0.2.0.0

- Add `Spawn` to `Process`
- Merge `MessagePassing` and `Process`
- Add initial test suite
- Fix shutdown error in `ForkIoScheduler`
- Rename `Dispatcher` to `Scheduler`
- Add `receiveLoop` function to `Process`
- Change `Api.Server` `serve` to loop instead of handling just one request
- Allow combining multiple `ApiHandler` such that one process can handle
  multiple APIs

## 0.1.3.0

- Rename and split `GenServer` to `Api`, `Api.Client`, `Api.Server`
- Add `registerServer`, `callRegistered` and `castRegistered`
- Remove the variant of `cast` that returns a boolean

## 0.1.2.2

- Try to fix version bounds for hackage

## 0.1.2.1

- Add more documentation
- Simplify Scheduler API
- Make more exception safe

## 0.1.2.0

- Add Observer module
- Implement Exception handling
- Improve Scheduler shutdown
- Add logging support via the logging-effect library

## 0.1.1.0

- Substantial API reorganization
- Rename/Move modules

## 0.1.0.1

- Stack/Cabal/Github Cosmetics
- Travis build job

## 0.1.0.0

- Initial Version
