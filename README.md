# extensible-effects-concurrent

[![Build Status](https://travis-ci.org/sheyll/extensible-effects-concurrent.svg?branch=master)](https://travis-ci.org/sheyll/extensible-effects-concurrent)

[![Hackage](https://img.shields.io/hackage/v/extensible-effects-concurrent.svg?style=flat)](http://hackage.haskell.org/package/extensible-effects-concurrent)

## From Erlang to Haskell

This project is an attempt to implement core ideas learned from the **Erlang/OTP**  
framework in Haskell using **[extensible-effects](http://hackage.haskell.org/package/extensible-effects)**.

This library sketches my personal history of working on a large, real world Erlang
application, trying to bring some of the ideas over to Haskell.

I know about cloud-haskell and transient, but I wanted something based on 
'extensible-effects', and I also wanted to deepen my understanding of it.

### Modeling an Application with Processes

The fundamental approach to modelling applications in Erlang is
based on the concept of concurrent, communicating processes.


### Example Code

```haskell
module Main where

import           Control.Eff
import           Control.Eff.Concurrent

main :: IO ()
main = defaultMain example

example :: Eff Effects ()
example = do
  person <- spawn "alice" alice
  replyToMe <- self
  sendMessage person replyToMe
  personName <- receiveMessage
  logInfo' ("I just met " ++ personName)

alice :: Eff Effects ()
alice = do
  logInfo "I am waiting for someone to ask me..."
  sender <- receiveMessage
  sendMessage sender ("Alice" :: String)
  logInfo' (show sender ++ " message received.")

```
This is taken from [example-4](./examples/example-4/Main.hs).


**Running** this example causes this output:

```text
DEBUG      no proc  scheduler loop entered                                       at ForkIOScheduler.hs:209
DEBUG        init!1 enter process                                                at ForkIOScheduler.hs:691
NOTICE       init!1 ++++++++ main process started ++++++++                       at ForkIOScheduler.hs:579
DEBUG       alice!2 enter process                                                at ForkIOScheduler.hs:691
INFO        alice!2 I am waiting for someone to ask me...                        at Main.hs:19
INFO        alice!2 !1 message received.                                         at Main.hs:22
DEBUG       alice!2 exit: Process finished successfully                          at ForkIOScheduler.hs:729
INFO         init!1 I just met Alice                                             at Main.hs:15
NOTICE       init!1 ++++++++ main process returned ++++++++                      at ForkIOScheduler.hs:581
DEBUG        init!1 exit: Process finished successfully                          at ForkIOScheduler.hs:729
DEBUG      no proc  scheduler loop returned                                      at ForkIOScheduler.hs:211
DEBUG      no proc  scheduler cleanup begin                                      at ForkIOScheduler.hs:205
NOTICE     no proc  cancelling processes: []                                     at ForkIOScheduler.hs:222
NOTICE     no proc  all processes cancelled                                      at ForkIOScheduler.hs:239
```

The mental model of the programming framework regards objects as **processes**
with an isolated internal state. 

**[Processes](http://hackage.haskell.org/package/extensible-effects-concurrent-0.25.0/docs/Control-Eff-Concurrent-Process.html)** are at the center of that contraption. All *actions*
happen in processes, and all *interactions* happen via messages sent
between processes. 

This is called **Message Passing Concurrency**;
in this library it is provided via the **`Process`** effect. 

The **`Process`** effect itself is just an *abstract interface*.

There are two schedulers, that *interpret* the `Process` effect:

- A *multi-threaded* scheduler, based on the `async`
- A *pure* single-threaded scheduler, based on coroutines

### Using the library

For convenience, it is enough to import one of three modules:

- [Control.Eff.Concurrent](http://hackage.haskell.org/package/extensible-effects-concurrent/docs/Control-Eff-Concurrent.html) for a multi threaded scheduler and `LoggingAndIo`
- [Control.Eff.Concurrent.Pure](http://hackage.haskell.org/package/extensible-effects-concurrent/docs/Control-Eff-Concurrent-Pure.html), for a single threaded scheduler and pure log capturing and otherwise no IO
- [Control.Eff.Concurrent.SingleThreaded](http://hackage.haskell.org/package/extensible-effects-concurrent/docs/Control-Eff-Concurrent-SingleThreaded.html), for a single threaded scheduler and totally impure logging via IO

### Process Life-Cycles and Interprocess Links

All processes except the first process are **`spawned`** by existing 
processes.

When a process **`spawns`** a new process they are independent apart from the fact that
the parent knows the process-id of the spawend child process.
 
Processes can **monitor** each other to be notified when a communication partner exits, 
potentially in unforseen ways.

Similarily processes may choose to mutually **link** each other.

That allows to model **trees** in which processes watch and start or
restart each other.

Because processes never share memory, the internal - possibly broken - state of 
a process is gone, when a process exits; hence restarting a process will not
be bothered by left-over, possibly inconsistent, state. 

### Higher Level Abstractions

Processes can receive only message of type `Dynamic`.

In order to leverage Haskells type-safety, a bit of support code is available.

There is a **data family** called **`Api`** allowing to model **calls** and 
**casts**, as well as event management and process supervision.

These facilities are very important to build **non-defensive**, **let-it-crash**
applications, resilient to runtime errors.   

### Additional services

Currently a **logging effect** is also part of the code base.

## Usage and Implementation

Should work with `stack`, `cabal` and `nix`. 

### Required GHC Extensions

In order to use the library you might need to activate some extension
in order to fight some ambiguous types, stemming from the flexibility to
choose different Scheduler implementations.

- AllowAmbiguousTypes
- TypeApplications


## Planned Features

- Stackage [![extensible-effects-concurrent LTS](http://stackage.org/package/extensible-effects-concurrent/badge/lts)](http://stackage.org/lts/package/extensible-effects-concurrent)

- Scheduler `ekg` Monitoring
