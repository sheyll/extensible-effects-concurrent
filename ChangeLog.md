# Changelog for extensible-effects-concurrent

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

### TODO

- Add `Kill` action to `Process`

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
