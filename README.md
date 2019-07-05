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

### Timers 

[The Timer module](src/Control/Eff/Concurrent/Process/Timer.hs) contains functions send messages after a time
has passed, and reiceive messages with timeouts.

### [The Protocol Metaphor](./src/Control/Eff/Concurrent/Protocol.hs)s

Processes can receive only message of type `Dynamic`.

In order to leverage Haskells type-safety, a bit of support code is available.

As the library carefully leaves the realm of untyped messages, it uses a **stateles protocol** as a metaphor.

The idea is to have a **typed process identifiers**.

The _type_ defines what messages can be received and if 
and what answer is sent back to the sender.

To have some value in the library for some real world applications, this metaphor should answer these questions:

1. What messages does a process accept?
2. When sending a certain message, should the sender wait for an answer?

Well, I do not have emperical evidence that these are the
most important things to use the type system for.

I can only back it with anectdotal evidence from my
professional work for Telcos in Erlang.

#### Protocol Phantom Type

In this library, the key to a _protocol_ is a single type,
that could even be a so called __phantom type__, i.e. a
type without any runtime values:

```haskell 

data MyPhantomType -- look mom, no constructors!! 

``` 

Such a type exists only for the type system.

It can only be used as a parameter to certain type constructors,
and for defining type class and type family instances.

#### Protocol Data Units

Messages that belong to a protocol are called __protocol data units (PDU)__.

#### Protocol Servers Endpoints

The `ProcessId` of a process identifies the messages box that
`receiveMessage` will use, when waiting for an incoming message.

While it defines _where_ the messages are collected, it does 
not restrict or inform about _what_ data is handled by a process.

An [Endpoint](https://hackage.haskell.org/package/extensible-effects-concurrent/docs/Control-Eff-Concurrent-Protocol.html#t:Endpoint) is a wrapper 
around the `ProcessId` that takes a _type parameter_.

The type does not have to have any values, it can be a phantom type.

This type serves only the tag the process as a server accepting messages identified by the [HasPdu](https://hackage.haskell.org/package/extensible-effects-concurrent/docs/Control-Eff-Concurrent-Protocol.html#t:HasPdu)   type class.

#### Server/Protocol Composability

Usually a protocol consists of some really custom PDUs and
some PDUs that more or less are found in many protocols,
like event listener registration and event notification.

It is therefore helpful to be able to compose protocols.

The machinery in this library allows to list several 
PDU instances understood by endpoints of a given protocol
phantom type.

#### Protocol Clients   

_Clients_ use a protocol by sending `Pdu`s indexed by 
some protocol phantom type to a server process.

Clients use `Endpoint`s to address these servers, and the
functions defined in [the corresponding module](https://hackage.haskell.org/package/extensible-effects-concurrent/docs/Control-Eff-Concurrent-Protocol-Client.html).

Most important are these two functions:  

* [cast](https://hackage.haskell.org/package/extensible-effects-concurrent/docs/Control-Eff-Concurrent-Protocol-Client.html#v:cast)
  to send fire-and-forget messages
   
* [call](https://hackage.haskell.org/package/extensible-effects-concurrent/docs/Control-Eff-Concurrent-Protocol-Client.html#v:cast)
  to send RPC-style requests and wait (block) for the responses.
  Also, when the server is not running, or crashes in 
  while waiting, the calling process is interrupted

#### Protocol Servers   

This library offers an API for defining practically safe to
 use __protocol servers__:

* [EffectfulServer](./src/Control/Eff/Concurrent/Protocol/EffectfulServer.hs) This modules defines the framework
  of a process, that has a callback function that is repeatedly
  called when ever a message was received.
  
  The callback may rely on any extra effects (extensible effects).    

* [StatefulServer](./src/Control/Eff/Concurrent/Protocol/StatefulServer.hs) A server based on the `EffectfulServer` that includes the definition of
  in internal state called __Model__, and some nice 
  helper functions to access the model. These functions
  allow the use of lenses. 
  Unlike the effectful server, the effects that the 
  callback functions can use are defined in this module.

* [CallbackServer](./src/Control/Eff/Concurrent/Protocol/CallbackServer.hs) A server based on the `EffectfulServer` that does not require a type class
   instance like the stateful and effectful servers do.
   It can be used to define _inline_ servers.
  
#### Events and Observers

A parameterized protocol for event handling is provided in 
these modules:

* [Observer](./src/Control/Eff/Concurrent/Protocol/Observer.hs) Defines the types and helper functions to
  integrate  
 

#### Supervisors and Watchdogs

A key part of a robust system is that the interaction between
a client and a server happens not only through processIds wrapped
in some endpoint, but also by using symbolic name or ID via
a process broker.

TODO write about Supervisor

TODO write about Watchdog


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
