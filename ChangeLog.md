# Changelog for extensible-effects-concurrent

## 0.2.0.3

  * Improve `LogChannel` API
  * Reorganize unit tests
  * Hopefully tune travis ci test parameter enough to get a stable build result

## 0.2.0.2

  * Fix minor `stack upload` complaints about the cabal file

## 0.2.0.1

  * Simplify IO Exception handling in `ForkIoScheduler`,
  * Add many unit tests for exception, exit and shutdown

## 0.2.0.0

  * Add `Spawn` to `Process`
  * Merge `MessagePassing` and `Process`
  * Add initial test suite
  * Fix shutdown error in `ForkIoScheduler`
  * Rename `Dispatcher` to `Scheduler`
  * Add `receiveLoop` function to `Process`
  * Change `Api.Server` `serve` to loop instead of handling just one request
  * Allow combining multiple `ApiHandler` such that one process can handle
    multiple APIs

### TODO
  * Add `Kill` action to `Process`

## 0.1.3.0

  * Rename and split `GenServer` to `Api`, `Api.Client`, `Api.Server`
  * Add `registerServer`, `callRegistered` and `castRegistered`
  * Remove the variant of `cast` that returns a boolean

## 0.1.2.2
  * Try to fix version bounds for hackage

## 0.1.2.1
  * Add more documentation
  * Simplify Scheduler API
  * Make more exception safe

## 0.1.2.0

  * Add Observer module
  * Implement Exception handling
  * Improve Scheduler shutdown
  * Add logging support via the logging-effect library

## 0.1.1.0

  * Substantial API reoganization
  * Rename/Move modules

## 0.1.0.1

  * Stack/Cabal/Github Cosmetics
  * Travis build job

## 0.1.0.0

  * Initial Version
