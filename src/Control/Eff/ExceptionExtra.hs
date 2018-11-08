{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- | Add-ons to 'Control.Eff.Exception' and 'Control.Excepion'
module Control.Eff.ExceptionExtra
  ( liftTry
  , maybeThrow
  , module Eff
  )
where

import qualified Control.Exception.Safe        as Safe
import qualified Control.Monad.Catch           as Catch
import           Control.Eff
import           Control.Eff.Extend
import           Control.Eff.Lift
import           Control.Eff.Exception         as Eff
import           GHC.Stack
import           Control.Eff.Reader.Lazy       as Lazy
import           Control.Eff.Reader.Strict     as Strict

-- | Catch 'Safe.Exception' thrown by an effect.
liftTry
  :: forall e r a
   . (HasCallStack, Safe.Exception e, Lifted IO r)
  => Eff r a
  -> Eff r (Either e a)
liftTry m = (Right <$> m) `catchDynE` (return . Left)


-- | Very similar to 'Eff.liftEither' but for 'Maybe's. Unlike 'Eff.liftMaybe' this
-- will throw the given value (instead of using 'Eff.Fail').
maybeThrow :: Member (Eff.Exc x) e => x -> Maybe a -> Eff e a
maybeThrow x = Eff.liftEither . maybe (Left x) Right


-- * Orphan 'MonadThrow', 'MonadCatch' and 'MonadMask'   instances

-- ** Lazy Reader

instance Catch.MonadThrow (Eff e) => Catch.MonadThrow (Eff (Lazy.Reader x ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance Catch.MonadCatch (Eff e) => Catch.MonadCatch (Eff (Lazy.Reader x ': e)) where
  catch effect handler = do
    readerValue <- Lazy.ask @x
    let nestedEffects =
          Lazy.runReader readerValue effect
        nestedHandler exception =
          Lazy.runReader readerValue (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance Catch.MonadMask (Eff e) => Catch.MonadMask (Eff (Lazy.Reader x ': e)) where
  mask maskedEffect = do
    readerValue <- Lazy.ask @x
    raise
      (Catch.mask
        (\nestedUnmask -> Lazy.runReader
          readerValue
          (maskedEffect
            (\unmasked ->
              raise (nestedUnmask (Lazy.runReader readerValue unmasked))
            )
          )
        )
      )
  uninterruptibleMask maskedEffect = do
    readerValue <- Lazy.ask @x
    raise
      (Catch.uninterruptibleMask
        (\nestedUnmask -> Lazy.runReader
          readerValue
          (maskedEffect
            (\unmasked ->
              raise (nestedUnmask (Lazy.runReader readerValue unmasked))
            )
          )
        )
      )
  generalBracket acquire release use = do
    readerValue <- Lazy.ask @x
    let
      lower :: Eff (Lazy.Reader x ': e) a -> Eff e a
      lower = Lazy.runReader readerValue
    raise
      (Catch.generalBracket
        (lower acquire)
        (((.).(.)) lower release)
        (lower . use))

-- ** Strict Reader

instance Catch.MonadThrow (Eff e) => Catch.MonadThrow (Eff (Strict.Reader x ': e)) where
  throwM exception = raise (Catch.throwM exception)

instance Catch.MonadCatch (Eff e) => Catch.MonadCatch (Eff (Strict.Reader x ': e)) where
  catch effect handler = do
    readerValue <- Strict.ask @x
    let nestedEffects =
          Strict.runReader readerValue effect
        nestedHandler exception =
          Strict.runReader readerValue (handler exception)
    raise (Catch.catch nestedEffects nestedHandler)

instance Catch.MonadMask (Eff e) => Catch.MonadMask (Eff (Strict.Reader x ': e)) where
  mask maskedEffect = do
    readerValue <- Strict.ask @x
    raise
      (Catch.mask
        (\nestedUnmask -> Strict.runReader
          readerValue
          (maskedEffect
            (\unmasked ->
              raise (nestedUnmask (Strict.runReader readerValue unmasked))
            )
          )
        )
      )
  uninterruptibleMask maskedEffect = do
    readerValue <- Strict.ask @x
    raise
      (Catch.uninterruptibleMask
        (\nestedUnmask -> Strict.runReader
          readerValue
          (maskedEffect
            (\unmasked ->
              raise (nestedUnmask (Strict.runReader readerValue unmasked))
            )
          )
        )
      )
  generalBracket acquire release use = do
    readerValue <- Strict.ask @x
    let
      lower :: Eff (Strict.Reader x ': e) a -> Eff e a
      lower = Strict.runReader readerValue
    raise
      (Catch.generalBracket
        (lower acquire)
        (((.).(.)) lower release)
        (lower . use))

-- ** Lifted IO

instance Catch.MonadThrow m => Catch.MonadThrow (Eff '[Lift m]) where
  throwM exception = lift (Catch.throwM exception)

instance Catch.MonadCatch m => Catch.MonadCatch (Eff '[Lift m]) where
  catch effect handler = do
    let nestedEffects = runLift effect
        nestedHandler exception = runLift (handler exception)
    lift (Catch.catch nestedEffects nestedHandler)

instance Catch.MonadMask m => Catch.MonadMask (Eff '[Lift m]) where
  mask maskedEffect = do
    lift
      (Catch.mask
        (\nestedUnmask -> runLift
          (maskedEffect
            (\unmasked -> lift (nestedUnmask (runLift unmasked))
            )
          )
        )
      )
  uninterruptibleMask maskedEffect = do
    lift
      (Catch.uninterruptibleMask
        (\nestedUnmask -> runLift
          (maskedEffect
            (\unmasked ->
              lift (nestedUnmask (runLift unmasked))
            )
          )
        )
      )
  generalBracket acquire release use = do
    lift
      (Catch.generalBracket
        (runLift acquire)
        (((.).(.)) runLift release)
        (runLift . use))
