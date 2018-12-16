# extensible-effects-concurrent

Message passing concurrency with 'forkIO' and 'extensible-effects' inspired by Erlang.

[![Build Status](https://travis-ci.org/sheyll/extensible-effects-concurrent.svg?branch=master)](https://travis-ci.org/sheyll/extensible-effects-concurrent)

[![Hackage](https://img.shields.io/hackage/v/extensible-effects-concurrent.svg?style=flat)](http://hackage.haskell.org/package/extensible-effects-concurrent)

Also included:

- Logging

- Memory Leak Free `forever`

## GHC Extensions

In order to use the library you might need to activate some extension
in order to fight some ambiguous types, stemming from the flexibility to
choose different Scheduler implementations.

- AllowAmbiguousTypes
- TypeApplications


## Example

```haskell
module Main where

import           Control.Eff
import           Control.Eff.Lift
import           Control.Eff.Concurrent
import           Data.Dynamic
import           Control.Concurrent
import           Control.DeepSeq

main :: IO ()
main = defaultMain
  (do
    lift (threadDelay 100000) -- because of async logging
    firstExample
    lift (threadDelay 100000) -- ... async logging
  )
    -- The SchedulerProxy paremeter contains the effects of a specific scheduler
    -- implementation.


newtype WhoAreYou = WhoAreYou ProcessId deriving (Typeable, NFData, Show)

firstExample
  :: (HasLogging IO q) => Eff (InterruptableProcess q) ()
firstExample = do
  person <- spawn
    (do
      logInfo "I am waiting for someone to ask me..."
      WhoAreYou replyPid <- receiveMessage px
      sendMessage replyPid "Alice"
      logInfo (show replyPid ++ " just needed to know it.")
    )
  me <- self
  sendMessage person (WhoAreYou me)
  personName <- receiveMessage
  logInfo ("I just met " ++ personName)


```

**Running** this example causes this output:
(_not entirely true because of async logging, but true enough_)

```text
2018-11-05T10:50:42 DEBUG     scheduler loop entered                                                   ForkIOScheduler.hs line 131
2018-11-05T10:50:42 DEBUG            !1 [ThreadId 11] enter process                                                            ForkIOScheduler.hs line 437
2018-11-05T10:50:42 NOTICE           !1 [ThreadId 11] ++++++++ main process started ++++++++                                   ForkIOScheduler.hs line 394
2018-11-05T10:50:42 DEBUG            !2 [ThreadId 12] enter process                                                            ForkIOScheduler.hs line 437
2018-11-05T10:50:42 INFO             !2 [ThreadId 12] I am waiting for someone to ask me...                                               Main.hs line 27
2018-11-05T10:50:42 INFO             !2 [ThreadId 12] !1 just needed to know it.                                                          Main.hs line 30
2018-11-05T10:50:42 DEBUG            !2 [ThreadId 12] returned                                                                 ForkIOScheduler.hs line 440
2018-11-05T10:50:42 INFO             !1 [ThreadId 11] I just met Alice                                                                    Main.hs line 35
2018-11-05T10:50:42 NOTICE           !1 [ThreadId 11] ++++++++ main process returned ++++++++                                  ForkIOScheduler.hs line 396
2018-11-05T10:50:42 DEBUG            !1 [ThreadId 11] returned                                                                 ForkIOScheduler.hs line 440
2018-11-05T10:50:42 DEBUG     scheduler loop returned                                                  ForkIOScheduler.hs line 133
2018-11-05T10:50:42 DEBUG     scheduler cleanup begin                                                  ForkIOScheduler.hs line 137
2018-11-05T10:50:42 NOTICE    cancelling processes: []                                                 ForkIOScheduler.hs line 149
2018-11-05T10:50:42 DEBUG     scheduler cleanup done                                                   ForkIOScheduler.hs line 141
```

## TODO

### Stackage

Still todo...

[![extensible-effects-concurrent LTS](http://stackage.org/package/extensible-effects-concurrent/badge/lts)](http://stackage.org/lts/package/extensible-effects-concurrent)


### Other

- Process Linking/Monitoring

- Scheduler `ekg` Monitoring

- Timers and Timeouts (e.g. in `receive`)

- Rename stuff
