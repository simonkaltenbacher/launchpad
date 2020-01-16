{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.CloudFormation
  ( deployStack
  , findStack
  , getStackStatus
  , trackStackStatus
  )
  where

import           Conduit                         (MonadResource)

import           Control.Concurrent              (forkIO, threadDelay)
import           Control.Exception               (Exception)
import           Control.Lens                    ((^?), ix, view)
import           Control.Lens.Setter             ((.~), (?~))
import           Control.Monad                   (liftM, void)
import           Control.Monad.Catch             (MonadCatch, MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS

import           Data.Foldable                    (find, fold)
import           Data.Function                    ((&))
import           Data.Maybe                       (fromMaybe, mapMaybe)
import           Data.Text                        (Text, unpack)

import           Formatting
import           Formatting.Clock                 (timeSpecs)

import           GHC.Generics                     (Generic)

import           LaunchPad.Config

import qualified Network.AWS.CloudFormation as CF
import           Network.AWS.Data                 (toText)
import qualified Network.AWS.S3 as S3

import           Path

import           Relude                           hiding (toText)

import           System.Clock
import           System.Console.ANSI              (setCursorColumn)
import           System.IO                        (hFlush, stdout)


newtype StackId = StackId { unStackId :: Text }
  deriving (Eq, Generic, Show)

newtype ChangeSetName = ChangeSetName { unChangeSetName :: Text }
  deriving (Eq, Generic, Show)

newtype ChangeSetId = ChangeSetId { unChangeSetId :: Text }
  deriving (Eq, Generic, Show)

type AWSConstraint' m = (MonadThrow m, MonadCatch m, MonadResource m, MonadReader Config m)

data InvalidResponseError = InvalidResponseError Text
  deriving (Eq, Show)

instance Exception InvalidResponseError

data InvalidStackStatusError = InvalidStackStatusError Text
  deriving (Eq, Show)

instance Exception InvalidStackStatusError

data StackNotFoundError = StackNotFoundError Text
  deriving (Eq, Show)

instance Exception StackNotFoundError

findStack :: MonadThrow m => StackName -> [Stack] -> m Stack
findStack stackName = maybe (throwM $ err) pure . find ((== stackName) . _stackName)
  where
    err = StackNotFoundError $ "Unable to find Stack " <> unStackName stackName <> "."

deployStack :: AWSConstraint' m => Bool -> Stack -> m StackId
deployStack disableRollback (stack @ Stack{..}) = do
  putTextLn $ "Uploading templates"
  mapM_ uploadTemplate (listTemplateIds stack)
  templateBucketName <- asks _templateBucketName
  putTextLn $ "Deploying stack "
    <> (unStackName _stackName)
    <> " to deployment environment "
    <> _deplEnv
  createStack disableRollback stack
  {-
  resp <- trying _ServiceError describeStack
  case resp of
    Left  _ -> createStack disableRollback stack
    Right _ -> updateStack stack
  -}

createStack :: AWSConstraint' m => Bool -> Stack -> m StackId
createStack disableRollback Stack{..} =
    handleResp =<< send . createReq =<< asks _templateBucketName
  where
    createReq templateBucketName =
      CF.createStack (unStackName _stackName)
        & CF.csCapabilities    .~ [CF.CapabilityNamedIAM]
        & CF.csDisableRollback ?~ disableRollback
        & CF.csParameters      .~ fmap (translateParam templateBucketName) _stackParams
        & CF.csTemplateURL     ?~ genS3Url templateBucketName _stackTemplateId

    handleResp
      = maybe (throwM $ InvalidResponseError "Received invalid response") (pure . StackId)
      . view CF.csrsStackId

createChangeSet :: AWSConstraint' m => ChangeSetName -> CF.ChangeSetType -> Stack -> m ChangeSetId
createChangeSet csName csType Stack{..} =
    handleResp =<< send . createReq =<< asks _templateBucketName
  where
    createReq templateBucketName =
      CF.createChangeSet (unStackName _stackName) (unChangeSetName csName)
        & CF.ccsChangeSetType ?~ csType
        & CF.ccsCapabilities  .~ [CF.CapabilityNamedIAM]
        & CF.ccsParameters    .~ fmap (translateParam templateBucketName) _stackParams
        & CF.ccsTemplateURL   ?~ genS3Url templateBucketName _stackTemplateId

    handleResp
      = maybe (throwM $ InvalidResponseError "Received invalid response") (pure . ChangeSetId)
      . view CF.ccsrsId

updateStack :: AWSConstraint' m => Stack -> m ()
updateStack Stack{..} =
    void . send . createReq =<< asks _templateBucketName
  where
    createReq templateBucketName =
      CF.updateStack (unStackName _stackName)
        & CF.usCapabilities .~ [CF.CapabilityNamedIAM]
        & CF.usParameters   .~ fmap (translateParam templateBucketName) _stackParams
        & CF.usTemplateURL  ?~ genS3Url templateBucketName _stackTemplateId

uploadTemplate :: AWSConstraint' m => TemplateId -> m ()
uploadTemplate tid = do
    putText $ unTemplateId tid <> "... "
    liftIO $ hFlush stdout
    templateDir <- asks _templateDir
    templateBucketName <- asks _templateBucketName
    body <- readBody templateDir tid
    send $ S3.putObject
      (S3.BucketName templateBucketName)
      (S3.ObjectKey $ unTemplateId tid)
      body
    putTextLn $ "DONE"
  where
    readBody templateDir
      = liftM (toBody . toHashed)
      . (=<<) (liftIO . readFile . toFilePath)
      . genLocalPath templateDir

describeChangeSet :: AWSConstraint' m => ChangeSetName -> m CF.DescribeChangeSetResponse
describeChangeSet = send . CF.describeChangeSet . unChangeSetName

describeStack :: AWSConstraint' m => StackName -> m CF.Stack
describeStack = (=<<) handleResp . send . createReq
  where
    createReq = flip (CF.dStackName ?~) CF.describeStacks . unStackName

    handleResp
      = maybe (throwM $ InvalidStackStatusError "Invalid stack status") pure
      . (^? CF.dsrsStacks . ix 0)

getStackStatus :: AWSConstraint' m => StackName -> m CF.StackStatus
getStackStatus = fmap (view CF.sStackStatus) . describeStack

trackStackStatus :: AWSConstraint' m => StackName -> m ()
trackStackStatus stackName = do
  start <- liftIO $ getTime Monotonic
  stackStatus <- getStackStatus stackName
  stackStatusVar <- liftIO newEmptyMVar 
  conf <- ask
  liftIO . void . forkIO $ runResourceT . runAWST conf $ pollStackStatus stackStatusVar stackName
  reportStatus stackStatusVar start stackStatus

pollStackStatus :: AWSConstraint' m => MVar CF.StackStatus -> StackName -> m ()
pollStackStatus stackStatusVar stackName = loop
  where
    loop = do
      nSecondDelay 10
      stackStatus <- getStackStatus stackName
      liftIO $ putMVar stackStatusVar stackStatus
      when (isInProgress stackStatus) loop

reportStatus :: MonadIO m => MVar CF.StackStatus -> TimeSpec -> CF.StackStatus -> m ()
reportStatus stackStatusVar start stackStatus = liftIO $ loop stackStatus
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
      else putTextLn ""

nSecondDelay :: MonadIO m => Int -> m ()
nSecondDelay n = liftIO $ threadDelay (n * 1000000)

isInProgress :: CF.StackStatus -> Bool
isInProgress CF.SSCreateInProgress                = True
isInProgress CF.SSUpdateInProgress                = True
isInProgress CF.SSUpdateCompleteCleanupInProgress = True
isInProgress _                                    = False

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