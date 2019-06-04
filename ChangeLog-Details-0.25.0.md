# 0.25.0 Effect Aliases and Module Structure Improvements 

- Introduce `type HasProcesses q r = (SetMember Process (Process q) r, Member Interrupts r)`

- Rename `InterruptableProcess` to `Processes` defined
  as `type Processes x = Interrupts : SafeProcesses x`

- Rename `ConsProcess` to `SafeProcesses` defined as
  `type SafeProcesses x = Process x : x`

- Remove/Hide `SchedulerIO`

- Remove/Hide `HasSchedulerIO`

- Rename `EffectfulServer.Effects` to `EffectfulServer.ServerEffects` 

- Add new **modules**:  
      
    - Add new re-exporting module for the single threaded scheduler with an `Effects` 
      alias and a `schedule` function, and re-exports from 
      `Control.Eff.Concurrent` and `Control.Eff.Log`:
     
          module Control.Eff.Concurrent.Pure
          ...
          
          type Effects = Processes BaseEffects
          
          type SafeEffects = SafeProcesses BaseEffects
    
          type BaseEffects = [Logs, LogWriterReader PureLogWriter]
          
          type HasBaseEffects e = BaseEffects <:: e 
          
          schedule :: Eff Effects a -> Either (Interrupt NoRecovery) a
          ...
          defaultMain :: Eff Effects () -> IO ()
          ...
          
    - Single threaded scheduler with IO with an `Effects` 
      alias and a `schedule` function, and re-exports from 
      `Control.Eff.Concurrent` and `Control.Eff.Log`: 
     
          module Control.Eff.Concurrent.Single
          ...
    
          type Effects = Processes BaseEffects
          
          type SafeEffects = SafeProcesses BaseEffects
    
          type BaseEffects = [Logs, LogWriterReader PureLogWriter] 
    
          type HasBaseEffects e = BaseEffects <:: e 
    
          schedule :: Eff Effects a -> Either (Interrupt NoRecovery) a
          ...
          defaultMain :: Eff Effects () -> IO ()
          ...
    
    - Multi threaded scheduler with an `Effects` alias and a `schedule` function, and re-exports from 
      `Control.Eff.Concurrent` and `Control.Eff.Log`: 
     
          module Control.Eff.Concurrent.Multi
          ...
          
          type Effects = Processes BaseEffects
          
          type SafeEffects = SafeProcesses BaseEffects
          
          type BaseEffects = Reader SchedulerState ': LoggingAndIo
          
          type HasBaseEffects e = BaseEffects <:: e
          
          schedule :: Eff Effects () -> Eff LoggingAndIo ()
          ...
          defaultMain :: Eff Effects () -> IO ()
          ...
