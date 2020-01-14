{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.CloudFormation
  ( deployStack
  , getStackStatus
  , trackStackStatus
  )
  where

import           Conduit                         (MonadResource)

import           Control.Concurrent
import           Control.Concurrent.MVar
import           Control.Exception               (Exception)
import           Control.Lens                    ((^?), ix, view)
import           Control.Lens.Setter             ((.~), (?~))
import           Control.Monad                   (liftM, void)
import           Control.Monad.Catch             (MonadCatch, MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS

import           Data.ByteString                  (readFile)
import           Data.Foldable                    (find, fold)
import           Data.Function                    ((&))
import           Data.List                        (intersperse)
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Text                        (pack, Text, unpack)
import           Data.Text.IO                     (putStr, putStrLn)

import           Formatting
import           Formatting.Clock                 (timeSpecs)

import           LaunchPad.Type

import qualified Network.AWS.CloudFormation as CF
import           Network.AWS.Data                 (toText)
import qualified Network.AWS.S3 as S3

import           Path

import           Prelude                          hiding (putStr, putStrLn, readFile)

import           System.Clock
import           System.Console.ANSI              (setCursorColumn)
import           System.IO                        (hFlush, stdout)


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
  createStack stack

createStack :: AWSConstraint' m => Stack -> m StackId
createStack Stack{..} = handleResp =<< send . createReq =<< asks _templateBucketName
  where
    createReq templateBucketName =
      CF.createStack (_deplEnv <> "-" <> _stackName)
        & CF.csCapabilities    .~ [CF.CapabilityNamedIAM]
        & CF.csDisableRollback ?~ True
        & CF.csParameters      .~ fmap (translateParam templateBucketName) _stackParams
        & CF.csTemplateURL     ?~ genS3Url templateBucketName _stackTemplateId

    handleResp
      = maybe (throwM $ CreateStackError "Received invalid response") (pure . StackId)
      . view CF.csrsStackId

uploadTemplate :: AWSConstraint' m => TemplateId -> m ()
uploadTemplate tid = do
    liftIO $ putStr $ unTemplateId tid <> "... "
    liftIO $ hFlush stdout
    templateDir <- asks _templateDir
    templateBucketName <- asks _templateBucketName
    body <- readBody templateDir tid
    send $ S3.putObject
      (S3.BucketName templateBucketName)
      (S3.ObjectKey $ unTemplateId tid)
      body
    liftIO $ putStrLn $ "DONE"
  where
    readBody templateDir
      = liftM (toBody . toHashed)
      . (=<<) (liftIO . readFile . toFilePath)
      . genLocalPath templateDir

describeStack :: AWSConstraint' m => StackId -> m CF.Stack
describeStack = (=<<) handleResp . send . createReq
  where
    createReq = flip (CF.dStackName ?~) CF.describeStacks . unStackId

    handleResp
      = maybe (throwM $ InvalidStackStatusError "Invalid stack status") pure
      . (^? CF.dsrsStacks . ix 0)

getStackStatus :: AWSConstraint' m => StackId -> m CF.StackStatus
getStackStatus = fmap (view CF.sStackStatus) . describeStack

trackStackStatus :: AWSConstraint' m => StackId -> m ()
trackStackStatus stackId = do
  start <- liftIO $ getTime Monotonic
  stackStatus <- getStackStatus stackId
  stackStatusVar <- liftIO newEmptyMVar 
  conf <- ask
  liftIO . void . forkIO $ runResourceT . runAWST conf $ pollStackStatus stackId stackStatusVar
  reportStatus start stackStatus stackStatusVar

pollStackStatus :: AWSConstraint' m => StackId -> MVar CF.StackStatus -> m ()
pollStackStatus stackId stackStatusVar = loop
  where
    loop = do
      nSecondDelay 10
      stackStatus <- getStackStatus stackId
      liftIO $ putMVar stackStatusVar stackStatus
      when (isInProgress stackStatus) loop

reportStatus :: MonadIO m => TimeSpec -> CF.StackStatus -> MVar CF.StackStatus -> m ()
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

isInProgress :: CF.StackStatus -> Bool
isInProgress CF.SSCreateInProgress                = True
isInProgress CF.SSUpdateInProgress                = True
isInProgress CF.SSUpdateCompleteCleanupInProgress = True
isInProgress _                                    = False

findStack :: MonadThrow m => Text -> [Stack] -> m Stack
findStack stackName = maybe (throwM $ err) pure . find ((== stackName) . _stackName)
  where
    err = StackNotFoundError $ "Unable to find Stack " <> stackName <> "."

listTemplateIds :: Stack -> [TemplateId]
listTemplateIds Stack{..} = _stackTemplateId : mapMaybe extract _stackParams
  where
    extract (Param _ (PTemplateId tid)) = pure tid
    extract _                           = Nothing

translateParam :: Text -> Param -> CF.Parameter
translateParam templateBucketName (Param pname pvalue) =
    CF.parameter
      & CF.pParameterKey   ?~ pname
      & CF.pParameterValue ?~ evalParamExpr pvalue
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