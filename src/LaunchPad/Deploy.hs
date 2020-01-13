{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.Deploy
  ( deployStack
  , getStackStatus
  , trackStackStatus
  )
  where

import Conduit                    (MonadResource)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception          (Exception)
import Control.Lens               ((^?), ix, view)
import Control.Lens.Setter        ((.~), (?~))
import Control.Monad              (liftM, void)
import Control.Monad.Catch        (MonadCatch, MonadThrow, throwM)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.AWS

import Data.ByteString            (readFile)
import Data.Foldable              (find, fold)
import Data.Function              ((&))
import Data.List                  (intersperse)
import Data.Maybe                 (fromMaybe, mapMaybe)
import Data.Text                  (pack, Text, unpack)
import Data.Text.IO               (putStr, putStrLn)

import Formatting
import Formatting.Clock           (timeSpecs)

import LaunchPad.Type

import Network.AWS.CloudFormation hiding (Stack)
import Network.AWS.Data           (toText)
import Network.AWS.S3

import Path

import Prelude                    hiding (putStr, putStrLn, readFile)

import System.Clock
import System.Console.ANSI        (setCursorColumn)
import System.IO                  (hFlush, stdout)


type AWSConstraint' m = (MonadThrow m, MonadCatch m, MonadResource m, MonadReader Config m)

data StackNotFoundError = StackNotFoundError Text
  deriving (Eq, Show)

data CreateStackError = CreateStackError Text
  deriving (Eq, Show)

data InvalidStackStatusError = InvalidStackStatusError Text
  deriving (Eq, Show)

instance Exception StackNotFoundError
instance Exception CreateStackError
instance Exception InvalidStackStatusError

deployStack :: AWSConstraint' m => Text -> m StackId
deployStack stackName = do
  stack <- findStack stackName =<< asks _stacks
  liftIO $ putStrLn $ "Uploading templates"
  mapM_ uploadTemplate (listTemplateIds stack)
  templateBucketName <- asks _templateBucketName
  liftIO $ putStrLn $ "Deploying stack " <> stackName <> " to deployment environment " <> _deplEnv stack
  performCreateStack stack

performCreateStack :: AWSConstraint' m => Stack -> m StackId
performCreateStack Stack{..} = handleResp =<< send . createReq =<< asks _templateBucketName
  where
    createReq templateBucketName =
      createStack (_deplEnv <> "-" <> _stackName)
        & csCapabilities    .~ [CapabilityNamedIAM]
        & csDisableRollback ?~ True
        & csParameters      .~ fmap (translateParam templateBucketName) _stackParams
        & csTemplateURL     ?~ genS3Url templateBucketName _stackTemplateId

    handleResp
      = maybe (throwM $ CreateStackError "Received invalid response") (pure . StackId)
      . view csrsStackId

uploadTemplate :: AWSConstraint' m => TemplateId -> m ()
uploadTemplate tid = do
    liftIO $ putStr $ unTemplateId tid <> "... "
    liftIO $ hFlush stdout
    templateDir <- asks _templateDir
    templateBucketName <- asks _templateBucketName
    body <- readBody templateDir tid
    send $ putObject
      (BucketName templateBucketName)
      (ObjectKey $ unTemplateId tid)
      body
    liftIO $ putStrLn $ "DONE"
  where
    readBody templateDir
      = liftM (toBody . toHashed)
      . (=<<) (liftIO . readFile . toFilePath)
      . genLocalPath templateDir

getStackStatus :: AWSConstraint' m => StackId -> m StackStatus
getStackStatus = (=<<) handleResp . send . createReq
  where
    createReq = flip (dStackName ?~) describeStacks . unStackId
 
    handleResp
      = maybe (throwM $ InvalidStackStatusError "Invalid stack status") pure
      . (^? dsrsStacks . ix 0 . sStackStatus)

trackStackStatus :: AWSConstraint' m => StackId -> m ()
trackStackStatus stackId = do
  start <- liftIO $ getTime Monotonic
  stackStatus <- getStackStatus stackId
  stackStatusVar <- liftIO newEmptyMVar 
  conf <- ask
  liftIO . void . forkIO $ runResourceT . runAWST conf $ pollStackStatus stackId stackStatusVar
  reportStatus start stackStatus stackStatusVar

pollStackStatus :: AWSConstraint' m => StackId -> MVar StackStatus -> m ()
pollStackStatus stackId stackStatusVar = loop
  where
    loop = do
      nSecondDelay 10
      stackStatus <- getStackStatus stackId
      liftIO $ putMVar stackStatusVar stackStatus
      when (isInProgress stackStatus) loop

reportStatus :: MonadIO m => TimeSpec -> StackStatus -> MVar StackStatus -> m ()
reportStatus start stackStatus stackStatusVar = liftIO $ loop stackStatus
  where
    loop stackStatus = do
      end <- getTime Monotonic
      stackStatus' <- fromMaybe stackStatus <$> tryTakeMVar stackStatusVar
      setCursorColumn 0
      fprint (stext % " Elapsed time: " % (right 10 ' ' %. timeSpecs)) (toText stackStatus') start end
      hFlush stdout
      nSecondDelay 1
      if isInProgress stackStatus'
      then loop stackStatus'
      else putStrLn ""

nSecondDelay :: MonadIO m => Int -> m ()
nSecondDelay n = liftIO $ threadDelay (n * 1000000)

isInProgress :: StackStatus -> Bool
isInProgress SSCreateInProgress                = True
isInProgress SSUpdateInProgress                = True
isInProgress SSUpdateCompleteCleanupInProgress = True
isInProgress _                                 = False

findStack :: MonadThrow m => Text -> [Stack] -> m Stack
findStack stackName = maybe (throwM $ err) pure . find ((== stackName) . _stackName)
  where
    err = StackNotFoundError $ "Unable to find Stack " <> stackName <> "."

listTemplateIds :: Stack -> [TemplateId]
listTemplateIds Stack{..} = _stackTemplateId : mapMaybe extract _stackParams
  where
    extract (Param _ (PTemplateId tid)) = pure tid
    extract _                           = Nothing

translateParam :: Text -> Param -> Parameter
translateParam templateBucketName (Param pname pvalue) =
    parameter
      & pParameterKey   ?~ pname
      & pParameterValue ?~ evalParamExpr pvalue
  where
    evalParamExpr (PLit value)      = value
    evalParamExpr (PTemplateId tid) = genS3Url templateBucketName tid

genLocalPath :: MonadThrow m => Path Abs Dir -> TemplateId -> m (Path Abs File)
genLocalPath templateDir = fmap (templateDir </>) . parseRelFile . unpack . unTemplateId

genS3Url :: Text -> TemplateId -> Text
genS3Url templateBucketName tid = fold
  [ "https://"
  , templateBucketName
  , ".s3.eu-central-1.amazonaws.com/"
  , unTemplateId tid
  ]