{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LaunchPad.Wait
  ( await
  , FailedWaitConditionError
  , WaitCondition (..)
  , WaitResult (..)
  ) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (async, poll)
import           Control.Monad.Catch      (throwM)
import           Control.Monad.Trans.AWS  hiding (await)

import           Formatting
import           Formatting.Clock         (timeSpecs)

import           LaunchPad.Exception
import           LaunchPad.PrettyPrint

import           Relude.Custom

import qualified Streaming.Prelude as S

import           System.Clock
import           System.Console.ANSI      (setCursorColumn)

data WaitResult r
  = WaitSuccess r
  | WaitRetry
  | WaitFailure Text
  deriving (Eq, Show)

data WaitCondition a r = WaitCondition
  { _check       :: a -> Either Error (Rs a) -> WaitResult r
  , _frequency   :: Int
  , _waitMessage :: Text
  }

await :: (AWSConstraint c m, AWSRequest a, PrettyPrint m) => WaitCondition a r -> a -> m r
await WaitCondition{..} req = do
    start <- liftIO $ getTime Monotonic
    conf <- ask
    resp <- wait start =<< monitorCond conf
    return resp
  where
    monitorCond conf
      = liftIO
      . async
      . runResourceT
      . runAWST conf
      . S.effects
      . S.untilRight
      $ pollAws

    pollAws = do
      nSecondDelay _frequency
      check =<< trying _Error (send req)

    check res = case _check req res of
      WaitSuccess r     -> pure . Right $ r
      WaitRetry         -> pure $ Left ()
      (WaitFailure msg) -> throwM $ FailedWaitConditionError msg

    wait start ares = either throwM pure =<< loop <* putTextLn ""
      where
        loop = do
          end <- liftIO $ getTime Monotonic
          liftIO $ setCursorColumn 0
          putDocB . pretty $ sformat (stext % " - Elapsed time: " % (right 10 ' ' %. timeSpecs)) _waitMessage start end
          nSecondDelay 1
          maybe loop pure =<< liftIO (poll ares)

nSecondDelay :: MonadIO m => Int -> m ()
nSecondDelay n = liftIO $ threadDelay (n * 1000000)