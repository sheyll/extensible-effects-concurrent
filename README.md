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

## TODO

### Stackage

Still todo...

[![extensible-effects-concurrent LTS](http://stackage.org/package/extensible-effects-concurrent/badge/lts)](http://stackage.org/lts/package/extensible-effects-concurrent)


### Other

- Process Linking/Monitoring

- Scheduler `ekg` Monitoring

- Timers and Timeouts (e.g. in `receive`)

- Rename stuff
