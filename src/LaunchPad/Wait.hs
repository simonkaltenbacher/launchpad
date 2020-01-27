{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LaunchPad.Wait
  ( await
  , CheckResult (..)
  , FailedWaitConditionError
  , RecoverResult (..)
  , WaitCondition (..)
  ) where

import           Control.Concurrent       (threadDelay)
import           Control.Concurrent.Async (Async, async, poll)
import           Control.Monad.Catch      (throwM)
import           Control.Monad.Trans.AWS  hiding (await)

import           Formatting
import           Formatting.Clock         (timeSpecs)

import           LaunchPad.PrettyPrint

import           Relude

import qualified Streaming.Prelude as S

import           System.Clock
import           System.Console.ANSI      (setCursorColumn)

data CheckResult
  = CheckSuccess
  | CheckRetry
  | CheckFailure Text
  deriving (Eq, Show)

data RecoverResult
  = RecoverRetry
  | RecoverFailure Text
  deriving (Eq, Show)

data WaitCondition a = WaitCondition
  { _check       :: a -> Rs a -> CheckResult
  , _recover     :: Error -> RecoverResult
  , _frequency   :: Int
  , _waitMessage :: Text
  }

data FailedWaitConditionError = FailedWaitConditionError Text
  deriving (Eq, Show)

instance Exception FailedWaitConditionError

await :: forall r m a. (AWSConstraint r m, AWSRequest a, PrettyPrint m) => WaitCondition a -> a -> m (Rs a)
await WaitCondition{..} req = do
    start <- liftIO $ getTime Monotonic
    conf <- ask
    resp <- wait start =<< monitorCond conf
    putTextLn ""
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
      either recover check =<< trying _Error (send req)

    recover err = case _recover err of
      RecoverRetry         -> pure $ Left ()
      (RecoverFailure msg) -> throwM $ FailedWaitConditionError msg

    check resp = case _check req resp of
      CheckSuccess       -> pure . pure $ resp
      CheckRetry         -> pure $ Left ()
      (CheckFailure msg) -> throwM $ FailedWaitConditionError msg

    wait :: TimeSpec -> Async (Rs a) -> m (Rs a)
    wait start aresp = either throwM pure =<< loop
      where
        loop = do
          end <- liftIO $ getTime Monotonic
          liftIO $ setCursorColumn 0
          putDocB . pretty $ sformat (stext % " - Elapsed time: " % (right 10 ' ' %. timeSpecs)) _waitMessage start end
          nSecondDelay 1
          maybe loop pure =<< liftIO (poll aresp)

nSecondDelay :: MonadIO m => Int -> m ()
nSecondDelay n = liftIO $ threadDelay (n * 1000000)