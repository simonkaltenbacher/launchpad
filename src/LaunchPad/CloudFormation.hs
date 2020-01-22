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
    mapM_ uploadTemplate (listTemplateIds stack)
    templateBucketName <- asks _templateBucketName
    putTextLn $ "Deploying stack "
      <> (unStackName _stackName)
      <> " to deployment environment "
      <> _deplEnv
    csType <- either (const CF.Create) (const CF.Update)
      <$> trying _ServiceError (describeStack _stackName)
    csId <- createChangeSet csName csType stack
    csCreateComplete <- await changeSetCreateComplete describeChangeSetReq 
    putTextLn "Created change set successfully"
    putTextLn "The following changes will be applied:"
    printChanges csCreateComplete
    putTextLn "Apply changes? [y/n]: "
    whenM ((== "y") <$> getLine) $ do
      executeChangeSet csName
      await stackCreateOrUpdateComplete describeStackReq
      putTextLn "Deployed stack successfully"
  where
    csName               = ChangeSetName . unStackName $ _stackName
    describeChangeSetReq = CF.describeChangeSet . unChangeSetName $ csName
    describeStackReq     = flip (CF.dStackName ?~) CF.describeStacks . unStackName $ _stackName

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

executeChangeSet :: AWSConstraint' m => ChangeSetName -> m ()
executeChangeSet = void . send . CF.executeChangeSet . unChangeSetName

getStackStatus :: AWSConstraint' m => StackName -> m CF.StackStatus
getStackStatus = fmap (view CF.sStackStatus) . describeStack

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
genLocalPath templateDir = fmap (templateDir </>) . parseRelFile . toString . unTemplateId

genS3Url :: Text -> TemplateId -> Text
genS3Url templateBucketName tid = fold
  [ "https://"
  , templateBucketName
  , ".s3.eu-central-1.amazonaws.com/"
  , unTemplateId tid
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
    _numAttempts = 100
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
    _numAttempts = 100
    _waitMessage = "Deploying stack"