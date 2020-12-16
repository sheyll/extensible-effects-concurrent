{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Main (main) where

import Control.DeepSeq
import Control.Eff
import Control.Eff.Concurrent
import Control.Monad (replicateM)
import qualified Criterion.Main as Criterion
import Criterion.Types
  ( bench,
    bgroup,
    nfAppIO,
  )
import Data.Dynamic
import GHC.Stack

main =
  Criterion.defaultMain
    [ bgroup
        "unidirectionalMessagePassing"
        [ bench
            ( "n="
                <> show noMessages
                <> ": "
                <> show senderNo
                <> " -> "                
                <> show receiverNo
            )
            ( nfAppIO
                unidirectionalMessagePassing
                (senderNo, noMessages, receiverNo)
            )
          | noMessages <- [100000],
            (senderNo, receiverNo) <-
              [ (1, 1000)
                --        (10, 100),
                --        (1, 1),
               -- (1000, 1)
              ]
        ]
    ]

mkTestMessage :: Int -> TestMessage
mkTestMessage !i =
  MkTestMessage
    ( "The not so very very very very very very very very very very very very very very very very very very very very very very very very " ++ show i,
      "large",
      "meeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeessssssssssssssssssssssssssssssssss" ++ show i,
      ( "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
        "ggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggggeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee",
        even i,
        123423421111111111111111111123234 * toInteger i
      )
    )

newtype TestMessage = MkTestMessage ([Char], [Char], [Char], ([Char], [Char], Bool, Integer))
  deriving newtype (Show, NFData, Typeable)

unidirectionalMessagePassing :: HasCallStack => (Int, Int, Int) -> IO ()
unidirectionalMessagePassing (!np, !nm, !nc) = defaultMainWithLogWriter noOpLogWriter $ do
  cs <- consumers
  ps <- producers cs
  awaitAllDown cs
  awaitAllDown ps
  where
    producers !cs =
      replicateM nc (spawn "producer" produce)
      where
        produce =
          mapM_
            (uncurry (flip sendMessage))
            ((,) <$> (mkTestMessage <$> [0 .. (nm `div` (nc * np)) - 1]) <*> cs)
    consumers = do
      replicateM nc (spawn "consumer" (consume (nm `div` nc)))
      where
        consume 0 = return ()
        consume workLeft = do
          (MkTestMessage !_msg) <- receiveMessage
          consume (workLeft - 1)

awaitAllDown :: [ProcessId] -> Eff Effects ()
awaitAllDown [] = return ()
awaitAllDown (p : rest) = do
  m <- monitor p
  _ <- receiveSelectedMessage (selectProcessDown m)
  awaitAllDown rest

