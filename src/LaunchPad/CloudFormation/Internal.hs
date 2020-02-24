{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module LaunchPad.CloudFormation.Internal
  ( AWSConstraint'
  , changeSetCreateComplete
  , ChangeSetId (..)
  , ChangeSetName (..)
  , createChangeSet
  , createStack
  , deleteResources
  , deleteStack
  , deleteStackComplete
  , describeStack
  , describeChangeSet
  , executeChangeSet
  , findStack
  , genLocalPath
  , genS3Url
  , listResourceIds
  , listStacks
  , MStack
  , stackCreateOrUpdateComplete
  , stackExists
  , StackId (..)
  , translateParam
  , uploadResource
  , module LaunchPad.Config
  )
  where

import           Conduit                          (MonadResource)

import           Control.Lens
import           Control.Lens.Setter              ((.~), (?~))
import           Control.Lens.Prism               (_Right)
import           Control.Monad.Catch              (MonadCatch, MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS          hiding (await)

import           Data.Foldable                    (find)
import           Data.Function                    ((&))
import           Data.Maybe                       (mapMaybe)

import           GHC.Generics                     (Generic)

import           LaunchPad.Config
import           LaunchPad.Exception
import           LaunchPad.PrettyPrint
import           LaunchPad.Wait

import qualified Network.AWS.CloudFormation as CF
import qualified Network.AWS.S3 as S3

import           Path

import           Relude                           hiding (toText)

import           Streaming
import qualified Streaming.Prelude as S

type MStack = CF.Stack

newtype StackId = StackId { unStackId :: Text }
  deriving (Eq, Generic, Pretty, Show)

newtype ChangeSetName = ChangeSetName { unChangeSetName :: Text }
  deriving (Eq, Generic, Pretty, Show)

newtype ChangeSetId = ChangeSetId { unChangeSetId :: Text }
  deriving (Eq, Generic, Pretty, Show)

type AWSConstraint' m = (MonadThrow m, MonadCatch m, MonadResource m, MonadReader Config m)

findStack :: MonadThrow m => StackName -> [Stack] -> m Stack
findStack stackName = maybe (throwM err) pure . find ((== stackName) . _stackName)
  where
    err = StackNotFoundException . renderDoc $ "Unable to find Stack " <> pretty stackName <> "."

createChangeSet :: AWSConstraint' m => ChangeSetName -> CF.ChangeSetType -> Stack -> m ChangeSetId
createChangeSet csName csType Stack{..} =
    handleResp =<< send . createReq =<< asks _resourceBucketName
  where
    createReq resBucketName =
      CF.createChangeSet (unStackName _stackName) (unChangeSetName csName)
        & CF.ccsChangeSetType ?~ csType
        & CF.ccsCapabilities  .~ [CF.CapabilityNamedIAM]
        & CF.ccsParameters    .~ fmap (translateParam resBucketName) _stackParams
        & CF.ccsRoleARN       .~ _roleArn
        & CF.ccsTemplateURL   ?~ genS3Url HttpsProtocol resBucketName _stackTemplateId

    handleResp
      = maybe (throwM $ InvalidResponseException "Received invalid response") (pure . ChangeSetId)
      . view CF.ccsrsId

createStack :: AWSConstraint' m => Bool -> Stack -> m StackId
createStack disableRollback Stack{..} =
    handleResp =<< send . createReq =<< asks _resourceBucketName
  where
    createReq resBucketName =
      CF.createStack (unStackName _stackName)
        & CF.csCapabilities    .~ [CF.CapabilityNamedIAM]
        & CF.csDisableRollback ?~ disableRollback
        & CF.csParameters      .~ fmap (translateParam resBucketName) _stackParams
        & CF.csRoleARN         .~ _roleArn
        & CF.csTemplateURL     ?~ genS3Url HttpsProtocol resBucketName _stackTemplateId

    handleResp
      = maybe (throwM $ InvalidResponseException "Received invalid response") (pure . StackId)
      . view CF.csrsStackId

uploadResource :: AWSConstraint' m => ResourceId -> Path Abs Dir -> m ()
uploadResource rid resourceDir = do
    void . send =<< createReq
      <$> asks _sseKmsKeyId
      <*> asks _serverSideEncryption
      <*> asks _resourceBucketName
      <*> readBody resourceDir rid
  where
    createReq sseKmsKeyId ssEnc resBucketName body = S3.putObject
      (S3.BucketName resBucketName)
      (S3.ObjectKey $ unResourceId rid)
      body
        & S3.poSSEKMSKeyId .~ sseKmsKeyId
        & S3.poServerSideEncryption .~ ssEnc

    readBody resourceDir
      = fmap (toBody . toHashed)
      . (=<<) (liftIO . readFile . toFilePath)
      . genLocalPath resourceDir

deleteResources :: AWSConstraint' m => [ResourceId] -> m ()
deleteResources rids = void . send =<< createReq <$> asks _resourceBucketName
  where
    createReq resBucketName = S3.deleteObjects (S3.BucketName resBucketName) resToDelete

    resToDelete = S3.delete'
      & S3.dObjects .~ fmap (S3.objectIdentifier . S3.ObjectKey . unResourceId) rids

describeChangeSet :: AWSConstraint' m => ChangeSetId -> m CF.DescribeChangeSetResponse
describeChangeSet = send . CF.describeChangeSet . unChangeSetId

describeStack :: AWSConstraint' m => StackName -> m MStack
describeStack = (=<<) handleResp . send . createReq
  where
    createReq = flip (CF.dStackName ?~) CF.describeStacks . unStackName

    handleResp
      = maybe (throwM $ InvalidStackStatusException "Invalid stack status") pure
      . (^? CF.dsrsStacks . ix 0)

listStacks :: AWSConstraint' m => Stream (Of (Either Stack MStack)) m ()
listStacks
    = S.mapM getOrPass
    . effect
    . fmap S.each
    $ asks _stacks
  where
    getOrPass (stack @ Stack{..}) = first (const stack) <$> trying _ServiceError (describeStack _stackName)

executeChangeSet :: AWSConstraint' m => ChangeSetId -> m ()
executeChangeSet = void . send . CF.executeChangeSet . unChangeSetId

deleteStack :: AWSConstraint' m => StackName -> m ()
deleteStack = void . send . CF.deleteStack . unStackName

stackExists :: AWSConstraint' m => StackName -> m Bool
stackExists
    = fmap (maybe False exists . (^? _Right . CF.staStackStatus))
    . trying _ServiceError . describeStack 
  where
    exists CF.SSDeleteComplete   = False
    exists CF.SSReviewInProgress = False
    exists _                     = True

listResourceIds :: Stack -> [ResourceId]
listResourceIds Stack{..} = _stackTemplateId : mapMaybe extract _stackParams
  where
    extract (Param _ (PResourceId _ rid)) = pure rid
    extract _                             = Nothing

translateParam :: Text -> Param -> CF.Parameter
translateParam resBucketName (Param pname pvalue) =
    CF.parameter
      & CF.pParameterKey   ?~ pname
      & CF.pParameterValue ?~ evalParamExpr pvalue
  where
    evalParamExpr (PLit value)            = value
    evalParamExpr (PResourceId proto rid) = genS3Url proto resBucketName rid

genLocalPath :: MonadThrow m => Path Abs Dir -> ResourceId -> m (Path Abs File)
genLocalPath resourceDir = fmap (resourceDir </>) . parseRelFile . toString . unResourceId

genS3Url :: Protocol -> Text -> ResourceId -> Text
genS3Url HttpsProtocol resBucketName rid
  =  "https://"
  <> resBucketName
  <> ".s3.eu-central-1.amazonaws.com/"
  <> unResourceId rid
genS3Url S3Protocol resBucketName rid
  =  "s3://"
  <> resBucketName
  <> "/"
  <> unResourceId rid

changeSetCreateComplete :: WaitCondition CF.DescribeChangeSet CF.DescribeChangeSetResponse
changeSetCreateComplete = WaitCondition {..}
  where
    _check _ = either (WaitFailure . renderDoc . pretty) handleResp

    handleResp resp = case resp ^. CF.dcscrsStatus of
      CF.CSSCreateComplete   -> WaitSuccess resp
      CF.CSSCreateInProgress -> WaitRetry
      CF.CSSCreatePending    -> WaitRetry
      _                      -> WaitFailure "Failed to create change set"

    _frequency = 2
    _waitMessage = "Creating change set"

deleteStackComplete :: WaitCondition CF.DescribeStacks ()
deleteStackComplete = WaitCondition {..}
  where
    _check _ = either handleFailure handleResp
    
    handleFailure
      = bool (WaitFailure "Failed to delete stack") (WaitSuccess ())
      . isJust
      . preview _ServiceError

    handleResp resp = case resp ^? CF.dsrsStacks . ix 0 . CF.staStackStatus of
      Just CF.SSDeleteComplete   -> WaitSuccess ()
      Just CF.SSDeleteInProgress -> WaitRetry
      _                          -> WaitFailure "Failed to delete stack"

    _frequency = 10
    _waitMessage = "Deleting stack"

stackCreateOrUpdateComplete :: Text -> WaitCondition CF.DescribeStacks CF.DescribeStacksResponse
stackCreateOrUpdateComplete _waitMessage = WaitCondition {..}
  where
    _check _ = either (WaitFailure . renderDoc . pretty) handleResp

    handleResp resp = case resp ^? CF.dsrsStacks . ix 0 . CF.staStackStatus of
      Just CF.SSCreateComplete                  -> WaitSuccess resp
      Just CF.SSUpdateComplete                  -> WaitSuccess resp
      Just CF.SSUpdateCompleteCleanupInProgress -> WaitSuccess resp
      Just CF.SSCreateInProgress                -> WaitRetry
      Just CF.SSUpdateInProgress                -> WaitRetry
      _                                         -> WaitFailure "Failed to create or update stack"

    _frequency = 10