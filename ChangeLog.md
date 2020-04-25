# Changelog for extensible-effects-concurrent

## 1.0.0

-  **[Timer](./src/Control/Eff/Concurrent/Process/Timer.hs)**
    - Allow timers to have custom initial debug log messages via

- **[Process](./src/Control/Eff/Concurrent/Process.hs)**
    - Split `Interrupt` into `ShutdownReason` and `InterruptReason`
    - Rename `isProcessDownInterrupt` to `isLinkedProcessCrashed`
    - Remove experimental type-safe `Receiver` type
    - Rename `StrictDynamic` to `Message`

- **[Logging](./src/Control/Eff/Log/Message.hs)**
    - Rename `LogMessage(...)` to `LogEvent(...)`
    - Introduce a newtype and a type class for log messages: `LogMsg` and `ToLogMsg`
        - An effort to move towards a specialized `ToLogMsg` class instead of
          relying on `Show`
        - The new type class `ToLogEventSender` is
          now used by the logging emitting functions
          like e.g.: `logDebug`, `logInfo`, etc.
          This allows to rewrite:

                logNotice ("dettaching: " <> pack (show deadBroker) <> " from: " <> pack (show broker))

          as:

                logNotice "dettaching: " deadBroker " from: " broker

    - Add `StringLogMsg` with the `MSG` constructor
    - Add `AsLogMsg` and `showAsLogMsg`
    - Remove `errorMessageIO`, `infoMessageIO` and `debugMessageIO`
    - Change the `appName` parameter type in some `LogWriter`s from `Text` to `String`

- **Protocol-Server**
    - All protocol server type class instances now expect `ToLogMsg` or `ToTypeLogMsg`
      instances where `Show` and `Typeable` was used before.
    - Remote the phantom type parameter of `Watchdog.CrashReport`

## 0.32.0

- **Protocol-Server**
    - Remove effect parameter from `StartArgument` and `Init`

- **ForkIO Scheduler**
    - Fix monitor reference leak
    - Shorten the process detail output,
      and return it from `getProcessState`

- **Async Logging**
    - Fix the asynchronous LogWriter so it does not stop logging after a flood of log messages


## 0.31.0

- **Logging**
   - Fix runtime crash caused by logging
     See: [#2](https://github.com/sheyll/extensible-effects-concurrent/issues/2)
    - Replace polymorphic `LogWriter` with
      a monomorphic one based on `IO`
    - Rename type aliases:
       - `LogsTo` -> `FilteredLogging`
       - `LogIo` -> `IoLogging`
    - Remove `Capturing` log writer
    - Remove `Capturing` log writer
    - Fix ghci log buffering issue
       [#1](https://github.com/sheyll/extensible-effects-concurrent/issues/1)

## 0.30.0
- Improve inline code documentation
- **Supervisor:**
    - Rename to **[Broker](./src/Control/Eff/Concurrent/Protocol/Broker.hs)**
    - Add the `ChildEvent` needed by the new **watchdog**
    - Add `callById` and `castById`
- [Process](./src/Control/Eff/Concurrent/Process.hs)
    - Introduce a new `Interrupt NoRecovery` clause: `ExitOtherProcessNotRunning`
    - Change the second parameter of `ProcessDown` from `InterruptOrShutdown` to `Interrupt NoRecovery`
    - Introduce new `Interrupt` reasons
      for all categories with an existential
      parameter, that must have `NFData`, `Show`
      and `Typeable` constraints.
    - Introduce a new timing primitive: `Delay`
- [StatefulServer](./src/Control/Eff/Concurrent/Protocol/StatefulServer.hs)
    - _Upgrade_ the associated **type alias** `Model`  to an associated **type**.
    - Add `mapEffects`
    - Add `coerceEffects`
- Export the `TimeoutMicros` constructor
- Add **[Watchdog](./src/Control/Eff/Concurrent/Protocol/Watchdog.hs)** a server that watches
  a **[Broker](./src/Control/Eff/Concurrent/Protocol/Broker.hs)** and restarts crashed
  processes registered at the broker.

- **[Logging](./src/Control/Eff/Log.hs)**
    - Add `logCallStack`
    - Add `logMultiLine`

-  **[Timer](./src/Control/Eff/Concurrent/Process/Timer.hs)**
    - Allow timers to have custom titles via:
        - `sendAfterWithTitle`
        - `startTimerWithTitle`
    - Switch to use the new `Delay` primitive

## 0.29.2
- Improve `Supervisor` API: Use `Init` from the effectful server as start
  argument type.

## 0.29.1
- Add more constraints to `Embeds`
- Improve the `CallbackServer`
- Add `LogWriterEffects`
- Rewrite `HandleLogWriter` so that the instance types have to be effects.
  TL,DR; This allows shorter type signatures than before.

## 0.29.0
- Remove the reply type parameter from `HasPdu`
- Make a new constraint `Embeds` that replaces `EmbedProtocol`
- Rename `EmbedProtocol` to `HasPduPrism`
- Add `EmbeddedPduList` to `HasPdu`
- Add embedded protocols example
- Rename and change `ServesProtocol` to `HasEndpointReader`

## 0.28.0
- Simplify `Protocol.Observer` registration API
- Rewrite `Protocol.Observer.Queue` API
- Add the `ProcessId` to the `ProcessDown` message

## 0.27.1
- Introduce `HasProcesses` and `HasSafeProcesses` everywhere

## 0.27.0
- Improve/fix `EmbedProtocol` type class
- Add a _this_ like parameter to the methods of `EffectfulServer`
- Rename `IsPdu` to `HasPdu`
- Remove `GenServer` from `EffectfulServer` and put it into a
  new module: `Control.Eff.Concurrent.Protocol.CallbackServer`
- Rename `Control.Eff.Concurrent.Protocol.Request` to `(...).Wrapper`

## 0.26.1
- Documentation fixes
- `Supervisor`: Don't start a new process when a process for a child-id exists

## 0.26.0
- Introduce `ReplyTarget`
- Change the `sendReply` signature to accept a `ReplyTarget`

## 0.25.1
- Add `castSingleton` and `callSingleton`, which use the `EndpointReader` and `EmbedProtocol` type class.
- Change `toObserver` to accept an Endpoint of a protocol that embeds `Observer x`
- Add `sendEmbeddedReply`
- Add `toEmbeddedEndpoint` and `fromEmbeddedEndpoint`
- Change `StatefulServer` class definition to not add `Processes` to the effects by default
- Add forgotten re-exports to `Concurrent`
- Fix the `NFData` instance for `Pdu (Observer o)`
- Put the `Pdu` data family inside of a new type class `IsPdu`

## 0.25.0
- Improve effect type aliases and module structure, [read the details here](./ChangeLog-Details-0.25.0.md).

## 0.24.3
- Add `EmbedProtocol` related function `toEmbeddedOrigin`

## 0.24.2
- Add more `EmbedProtocol` related functions:
    - `embedReplySerializer`
    - `embedRequestOrigin`
- Improve documentation for `EffectfulServer`
- Improve documentation for `StatefulServer`

## 0.24.1

- Add more `EmbedProtocol` tuple instances (4-tuple, 5-tuple)
- Make `Effectful.Server` instances composable (See the `GenServerTests` for an example)
  [more details in a seperate file](./ChangeLog-Details-0.24.1.md)
- Add `ProcessTitle` - every process now must have a short title text for logging
- Add `ProcessDetails` - every process can call `UpdateProcessDetails` to update
  its infos about the current state of it for debugging and error tracing purposes.
- Add `GetProcessState` to retreive the `ProcessDetails` for some other process.

## 0.24.0

- Get rid of the `PrettyTypeShow` constraint in `Tangible`
- Get rid of `LogWriterEffects` and the necessity for some `UndecidableInstances` that came with it
- Add `Server` module for `Api` handling via type classes
    - Add `Stateless`
    - Add `GenServer`
- Reimplement `Supervisor`

## 0.23.0

- Include the process id in the console and trace log renderer
- Add a **process supervisor** similar to Erlang/OTPs simple_one_for_one supervisor.
- Fix `SingleThreadedScheduler` process linking bug: A process shall not be interrupted
  when a linked process exits normally.
- Rename **ExitReason** to **Interrupt** and make the interrupt and exit handling
  API more robust.

## 0.22.1

- Fix duplicated content in RFC-5424 log message renderer

## 0.22.0

- Remove `SchedulerProxy` ruins

- Make message sending strict:

  Ensure that every message sent from one process to another
  is reduced to normal form by the sender.

    - Remove *all* lazy message selectors
    - Introduce a newtype wrapper `StrictDynamic` around `Dynamic`
      and export only a constructor that deeply evaluates the
      value to *rnf* before converting it to a `Dynamic`

- Change the `Server` API for better system *vitality*:

- Add `callWithTimeout`: A `call` over `IO` with a `Timeout` parameter

- Add more efficient log renderer:
    - `renderLogMessageBodyNoLocation`
    - `renderRFC5424NoLocation`

## 0.21.2

- Fix copy-paste error: Remove the `LogsTo` constraint from `withAsyncLogWriter`

## 0.21.1

- Remove dependency to the `socket` and `socket-unix` packages
  - they are marked as *broken* by in NixOS
  - the code based on `network` is much shorter

- Rewrite the UDP log writer to use `network`

- Rewrite the UnixSocket log writer to use `network`

## 0.21.0

- Add more log message renderers

    - Multiple extra time stamp formats
    - RFC3164

- Add IO log writer for unix domain sockets, e.g. `/dev/log`

- Add IO log writer for UDP

- Extract and simplify the async logger

- Extract and simplify the file log writers

## 0.20.0

- Rewrite Logging API so that usage is not as bloated

## 0.19.1

- Fix Travis build
- Fix typos
- Fix README

## 0.19.0

- Adapt to extensible-effects-concurrent 5.0.0.1
- Update to Stackage LTS-13.13
- Improve NIX expressions
- Rewrite the logging API
- Improve Documentation
- Add Examples

## 0.18.1

- Fix inappropriate `LinkedProcessCrashed` interrupt when a process exits with `NotRecovered ProcessFinished`

## 0.18.0

- Split-up and replace `spawnLinkObservationQueue` with a simpler (but more verbose) alternative

## 0.17.0

- Rename misspelled `spawnLinkObserverationQueue` to `spawnLinkObservationQueue`

## 0.16.1

- Export `ObserverRegistry` constructors for custom event registration handling

## 0.16.0

API Stabilization and cleanup release with major API changes.

- Replace `Control.Eff.Concurrent.Api.Server` with
   `Control.Eff.Concurrent.Api.Server2` and rename
   `Control.Eff.Concurrent.Api.Server2` to
   `Control.Eff.Concurrent.Api.Server`

- Rewrite `Observer` and related modules like `Observer.Queue`
  - Remove all type classes
  - Rely on `Server2`
  - Remove `CallBackObserver`
  - Remove the observer support code in `Server2`

- Remove the `SchedulerProxy` parameter and tell library users to enable `AllowAmbiguousTypes` and `TypeApplications`
  - Remove dependent support code like `HasScheduler`

## 0.15.0

- Add `Api` `Request` and `Reply` types
- Add `RequestOrigin` which can be used in `Server2` based Api servers to queue and defer replies to `Call`s

## 0.14.3

- Export the functions introduced in 0.14.2 in `Control.Eff.Concurrent`.

## 0.14.2

- Add `Server2` functions to spawn and _link_
- Add a `Server2` function to defer the reply to a `Call`s called:
  `handleCallsDeferred`

## 0.14.1

- Add Server2 based observation handling with `handleObservations`

## 0.14.0

- Fix/Improve Server2

## 0.13.2

- Add `ProcessFinished`
- Add `tryUninterrupted`
- Add simpler `Server2`

## 0.13.1

- Remove misguided `MonadCatch` constraints in the `ObservationQueueReader`
  functions, and use `Interrupts` instead

## 0.13.0

- Fix bad constraints in `Queue` observer

## 0.12.2

- Fix some compiler warnings

## 0.12.1

- Fix build errors with GHC-8.6

## 0.12.0

- Add implicit SchedulerProxy
- Add flushMessages
- Add receiving with timeout
- Add process `Link`ing and `Monitoring`.
- Make the distinction between recoverable and non-recoverable exit explicit in
  the type parameter of `ExitReason`, and introduce `interruptXXXX`
  functions in addition to `shutdownXXXX` functions, to throw recoverable exits.
- Merge `ShutdownRequest` and `ExitReason`
- Rename `receiveLoopSuchThat` to `receiveSelectedLoop`
- Pass the exit reason to the callback passed to `receiveSelectedLoop`
- Rename `receiveMessage` to `receiveAnyMessage`
- Rename `receiveAnyLoop` to `receiveAnyLoop`
- Pass the exit reason to the callback passed to `receiveAnyLoop`
- Rename `receiveMessage` to `receiveAnyMessage`
- Rename `receiveMessageAs` to `receiveMessage`
- Rename `receiveLoop` to `receiveLoop`
- Pass the exit reason to the callback passed to `receiveAnyLoop`
- Remove `SchedulerShuttingDown`
- Improve logging for exceptions in `ForkIOScheduler`
- Fix a bug in the logging system that caused all log filters to be forgotten
  when using unliftings such as `MonadBaseControl`, `MonadThrow`, `MonadCatch`
  and `MonadMask`
- Fix the scheduler shutdown to not always run into the cancellation timeout

## 0.11.1

- Fix a compilation error

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
- Add `ExitReason`
- Add `ProcessState`
- Add `ShutdownRequest` type
- Rewrite logging to be a `Reader` of a `LogWriter`
- Remove pure logging, the `Logs...` constraint must be
  accompanied by `Lifted IO` (or `MonadIO`) in many log functions
  most prominently `sendLogEvent`
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
  exiting the process
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
- Expose `setLogEventsTimestamp` and `setLogEventsThreadId`

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
- Rename `yieldProcess` to `executeAndResumeOrExit`
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
- Add `receiveAnyLoop` function to `Process`
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
