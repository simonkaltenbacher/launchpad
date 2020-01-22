{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.CloudFormation
  ( deployStack
  , findStack
  , getStackStatus
  )
  where

import           Conduit                          (MonadResource)

import           Control.Exception                (Exception)
import           Control.Lens
import           Control.Lens.Setter              ((.~), (?~))
import           Control.Monad.Catch              (MonadCatch, MonadThrow, throwM)
import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS          hiding (await)

import           Data.Foldable                    (find, fold)
import           Data.Function                    ((&))
import           Data.Maybe                       (mapMaybe)

import           GHC.Generics                     (Generic)

import           LaunchPad.Config
import           LaunchPad.Terminal
import           LaunchPad.Wait

import qualified Network.AWS.CloudFormation as CF
import qualified Network.AWS.S3 as S3
import qualified Network.AWS.Data as A

import           Path

import           Relude                           hiding (toText)

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
findStack stackName = maybe (throwM err) pure . find ((== stackName) . _stackName)
  where
    err = StackNotFoundError $ "Unable to find Stack " <> unStackName stackName <> "."

deployStack :: AWSConstraint' m => Bool -> Stack -> m ()
deployStack disableRollback (stack @ Stack{..}) = do
    putTextLn $ "Uploading templates"
    mapM_ uploadResource (listResourceIds stack)
    resBucketName <- asks _resourceBucketName
    putTextLn $ "Deploying stack "
      <> (unStackName _stackName)
      <> " to deployment environment "
      <> _deplEnv
    csType <- either (const CF.Create) (const CF.Update)
      <$> trying _ServiceError (describeStack _stackName)
    csId <- createChangeSet csName csType stack
    csCreateComplete <- await changeSetCreateComplete (describeChangeSetReq csId)
    putTextLn "Created change set successfully"
    putTextLn "The following changes will be applied:"
    printChanges csCreateComplete
    whenM (getConfirmation 3 parser "Do you want to continue? [y/n]: ") $ do
      executeChangeSet csId
      await stackCreateOrUpdateComplete describeStackReq
      putTextLn "Deployed stack successfully"
  where
    csName = ChangeSetName . unStackName $ _stackName

    describeChangeSetReq csId
      = CF.describeChangeSet
      . unChangeSetId $ csId

    describeStackReq
      = flip (CF.dStackName ?~) CF.describeStacks
      . unStackName $ _stackName

    parser "y" = Just True
    parser "n" = Just False
    parser _   = Nothing

createChangeSet :: AWSConstraint' m => ChangeSetName -> CF.ChangeSetType -> Stack -> m ChangeSetId
createChangeSet csName csType Stack{..} =
    handleResp =<< send . createReq =<< asks _resourceBucketName
  where
    createReq resBucketName =
      CF.createChangeSet (unStackName _stackName) (unChangeSetName csName)
        & CF.ccsChangeSetType ?~ csType
        & CF.ccsCapabilities  .~ [CF.CapabilityNamedIAM]
        & CF.ccsParameters    .~ fmap (translateParam resBucketName) _stackParams
        & CF.ccsTemplateURL   ?~ genS3Url resBucketName _stackTemplateId

    handleResp
      = maybe (throwM $ InvalidResponseError "Received invalid response") (pure . ChangeSetId)
      . view CF.ccsrsId

uploadResource :: AWSConstraint' m => ResourceId -> m ()
uploadResource rid = do
    putText $ unResourceId rid <> "... "
    liftIO $ hFlush stdout
    resourceDir <- asks _resourceDir
    send =<< createReq
      <$> asks _sseKmsKeyId
      <*> asks _serverSideEncryption
      <*> asks _resourceBucketName
      <*> readBody resourceDir rid
    putTextLn $ "DONE"
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

describeChangeSet :: AWSConstraint' m => ChangeSetId -> m CF.DescribeChangeSetResponse
describeChangeSet = send . CF.describeChangeSet . unChangeSetId

describeStack :: AWSConstraint' m => StackName -> m CF.Stack
describeStack = (=<<) handleResp . send . createReq
  where
    createReq = flip (CF.dStackName ?~) CF.describeStacks . unStackName

    handleResp
      = maybe (throwM $ InvalidStackStatusError "Invalid stack status") pure
      . (^? CF.dsrsStacks . ix 0)

executeChangeSet :: AWSConstraint' m => ChangeSetId -> m ()
executeChangeSet = void . send . CF.executeChangeSet . unChangeSetId

getStackStatus :: AWSConstraint' m => StackName -> m CF.StackStatus
getStackStatus = fmap (view CF.sStackStatus) . describeStack

listResourceIds :: Stack -> [ResourceId]
listResourceIds Stack{..} = _stackTemplateId : mapMaybe extract _stackParams
  where
    extract (Param _ (PResourceId rid)) = pure rid
    extract _                           = Nothing

translateParam :: Text -> Param -> CF.Parameter
translateParam resBucketName (Param pname pvalue) =
    CF.parameter
      & CF.pParameterKey   ?~ pname
      & CF.pParameterValue ?~ evalParamExpr pvalue
  where
    evalParamExpr (PLit value)      = value
    evalParamExpr (PResourceId rid) = genS3Url resBucketName rid

genLocalPath :: MonadThrow m => Path Abs Dir -> ResourceId -> m (Path Abs File)
genLocalPath resourceDir = fmap (resourceDir </>) . parseRelFile . toString . unResourceId

genS3Url :: Text -> ResourceId -> Text
genS3Url resBucketName rid = fold
  [ "https://"
  , resBucketName
  , ".s3.eu-central-1.amazonaws.com/"
  , unResourceId rid
  ]

printChanges :: MonadIO m => CF.DescribeChangeSetResponse -> m ()
printChanges = mapM_ printChange . listChanges
  where
    listChanges
      = toListOf
      $ CF.desrsChanges
      . traverse
      . CF.cResourceChange
      . _Just

    printChange change = putTextLn . fold . intersperse " " . fmap (fromMaybe "-") $
      [ view CF.rcLogicalResourceId $ change
      , view CF.rcResourceType $ change
      , fmap A.toText . view CF.rcAction $ change
      ]

changeSetCreateComplete :: WaitCondition CF.DescribeChangeSet
changeSetCreateComplete = WaitCondition {..}
  where
    _check _ resp = case resp ^. CF.desrsStatus of
      CF.CSSCreateComplete   -> CheckSuccess
      CF.CSSCreateInProgress -> CheckRetry
      CF.CSSCreatePending    -> CheckRetry
      _                      -> CheckFailure "Failed to create change set."

    _recover err = RecoverFailure . show $ err
    _frequency = 10
    _numAttempts = 150
    _waitMessage = "Creating change set"

stackCreateOrUpdateComplete :: WaitCondition CF.DescribeStacks
stackCreateOrUpdateComplete = WaitCondition {..}
  where
    _check _ resp = case resp ^? CF.dsrsStacks . ix 0 . CF.sStackStatus of
      Just CF.SSCreateComplete   -> CheckSuccess
      Just CF.SSUpdateComplete   -> CheckSuccess
      Just CF.SSCreateInProgress -> CheckRetry
      Just CF.SSUpdateInProgress -> CheckRetry
      _                          -> CheckFailure "Failed to create or update stack."

    _recover err = RecoverFailure . show $ err
    _frequency = 10
    _numAttempts = 150
    _waitMessage = "Deploying stack"