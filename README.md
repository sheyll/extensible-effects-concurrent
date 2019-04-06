# extensible-effects-concurrent

[![Build Status](https://travis-ci.org/sheyll/extensible-effects-concurrent.svg?branch=master)](https://travis-ci.org/sheyll/extensible-effects-concurrent)

[![Hackage](https://img.shields.io/hackage/v/extensible-effects-concurrent.svg?style=flat)](http://hackage.haskell.org/package/extensible-effects-concurrent)

## From Erlang to Haskell

Bring ideas from the Erlang eco-system to applications using 'extensible-effects'.

This library sketches my personal history of working on a large, real world Erlang
application, trying to bring some of the ideas over to Haskell.

I know about cloud-haskell and transient. 

But nature loves variety, and I wanted something based on 'extensible-effects'.

### Modeling an Application with Processes

The fundamental approach to modelling applications in Erlang is
based on the concept of concurrent, communicating processes, without
shared state. 

**`Processes`** are at the center of that contraption. All *actions*
happens in processes, and all *interactions* happen via messages sent
between processes. 

This is called **Message Passing Concurrency**;
in this library it is provided via the **`Process`** effect. 

The **`Process`** effect itself is just an *abstract interface*.

There are two schedulers, that *interpret* the `Process` effect:

- A *multi-threaded* scheduler, based on the `async`
- A *pure* single-threaded scheduler, based on coroutines

### Process Life-Cycles and Interprocess Links

When processes start new processes, the are *related* and often *linked*.

Process links form a trees.

When a parent process dies, the child processes dies as well.
If on the other hand a child dies, the parent will not die unless the
child *crashed*. 

A parent might also react by *restarting* the child from a defined starting
state.

Because processes never share memory, the internal - possibly broken - state of 
a process is gone, when a process exits; hence restarting a process will not
be bothered by left-over, possibly inconsistent, state. 

Erlang such parent processes are call *supervisor* processes in Erlang.

In order to build **supervision trees** the `Process` effect allows:

- Interrupting and killing Processes
- Process Monitoring
- Process Linking
- Timers and Timeouts

These facilities are very important to build **non-defensive**, **let-it-crash**
applications, resilient to runtime errors.   

Currently a custom **logging effect** is also part of the code base.

## Usage and Implementation

### Example Code

```haskell
module Main where

import           Control.Eff
import           Control.Eff.Concurrent
import           Data.Dynamic
import           Control.DeepSeq
import           GHC.Stack (HasCallStack)

main :: IO ()
main = defaultMain firstExample

newtype WhoAreYou = WhoAreYou ProcessId 
  deriving (Typeable, NFData, Show)

firstExample 
  :: (HasCallStack, Member Logs q) 
  => Eff (InterruptableProcess q) ()
firstExample = do
  person <- spawn
    (do
      logInfo "I am waiting for someone to ask me..."
      WhoAreYou replyPid <- receiveMessage
      sendMessage replyPid "Alice"
      logInfo (show replyPid ++ " just needed to know it.")
    )
  me <- self
  sendMessage person (WhoAreYou me)
  personName <- receiveMessage
  logInfo ("I just met " ++ personName)

```

**Running** this example causes this output:

```text
DEBUG     scheduler loop entered                                                   ForkIOScheduler.hs line 157
DEBUG            !1 enter process                                                            ForkIOScheduler.hs line 549
NOTICE           !1 ++++++++ main process started ++++++++                                   ForkIOScheduler.hs line 461
DEBUG            !2 enter process                                                            ForkIOScheduler.hs line 549
INFO             !2 I am waiting for someone to ask me...                                               Main.hs line 26
INFO             !2 !1 just needed to know it.                                                          Main.hs line 29
DEBUG            !2 exit normally                                                            ForkIOScheduler.hs line 568
INFO             !1 I just met Alice                                                                    Main.hs line 34
NOTICE           !1 ++++++++ main process returned ++++++++                                  ForkIOScheduler.hs line 463
DEBUG            !1 exit normally                                                            ForkIOScheduler.hs line 568
DEBUG     scheduler loop returned                                                  ForkIOScheduler.hs line 159
DEBUG     scheduler cleanup begin                                                  ForkIOScheduler.hs line 154
NOTICE    cancelling processes: []                                                 ForkIOScheduler.hs line 168
NOTICE    all processes cancelled                                                  ForkIOScheduler.hs line 179
```

### Required GHC Extensions

In order to use the library you might need to activate some extension
in order to fight some ambiguous types, stemming from the flexibility to
choose different Scheduler implementations.

- AllowAmbiguousTypes
- TypeApplications


## Planned Features

- Stackage [![extensible-effects-concurrent LTS](http://stackage.org/package/extensible-effects-concurrent/badge/lts)](http://stackage.org/lts/package/extensible-effects-concurrent)

- Scheduler `ekg` Monitoring
