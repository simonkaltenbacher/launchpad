{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.CloudFormation
  ( AWSConstraint'
  , runCreateStack
  , runDeleteStack
  , runDeployStack
  , runListStacks
  , findStack
  )
  where

import           Control.Lens
import           Control.Lens.Setter               ((?~))
import           Control.Monad.Catch               (throwM)
import           Control.Monad.Reader

import           Data.Time.Format

import           LaunchPad.CloudFormation.Internal
import           LaunchPad.Exception
import           LaunchPad.PrettyPrint
import           LaunchPad.Wait

import qualified Network.AWS.CloudFormation as CF
import           Network.AWS.Data.Time             (UTCTime)

import           Path

import           Relude                            hiding (toText)

import qualified Streaming.Prelude as S


runCreateStack :: (AWSConstraint' m, PrettyPrint m) => Bool -> StackName -> Path Abs Dir -> m ()
runCreateStack disableRollback stackName resourceDir  = do
  (stack @ Stack{..}) <- findStack stackName =<< asks _stacks
  uploadResources stack resourceDir
  withBlock ("Create stack " <> pretty _stackName) $ do
    createStack disableRollback stack
    await (stackCreateOrUpdateComplete "Creating stack") (createDescribeStackReq _stackName)
    reportSuccess "Stack creation complete"

runDeleteStack :: (AWSConstraint' m, PrettyPrint m) => StackName -> m ()
runDeleteStack stackName = do
    (stack @ Stack{..}) <- findStack stackName =<< asks _stacks
    withBlock ("Delete stack " <> pretty _stackName) $
      ifM (stackExists _stackName) deleteStack' err
  where
    deleteStack' = do
      deleteStack stackName
      await deleteStackComplete (createDescribeStackReq stackName)
      reportSuccess "Stack deletion complete"

    err = throwM . StackNotFoundException . renderDoc $ "Stack " <> pretty stackName <> " does not exist"

runDeployStack :: (AWSConstraint' m, PrettyPrint m) => StackName -> Path Abs Dir -> m ()
runDeployStack stackName resourceDir = do
    (stack @ Stack{..})  <- findStack stackName =<< asks _stacks
    uploadResources stack resourceDir 
    withBlock ("Deploy stack " <> pretty _stackName) $ do
      resBucketName <- asks _resourceBucketName
      csType <- bool CF.CSTCreate CF.CSTUpdate <$> stackExists _stackName
      csId <- createChangeSet csName csType stack
      csCreateComplete <- await changeSetCreateComplete (describeChangeSetReq csId)
      reportSuccess "Created change set"
      withBlock "The following changes will be applied:" (printChanges csCreateComplete)
      whenM (getConfirmation 3 parser "Do you want to continue? [y/n]: ") $ do
        executeChangeSet csId
        await (stackCreateOrUpdateComplete "Deploying stack") (createDescribeStackReq _stackName)
        reportSuccess "Stack deployment complete"
  where
    csName = ChangeSetName . unStackName $ stackName

    parser "y" = Just True
    parser "n" = Just False
    parser _   = Nothing

runListStacks :: (AWSConstraint' m, PrettyPrint m) => m ()
runListStacks
    = join
    . fmap (putDocBLn . tabulate . mkTable . cons header)
    . S.toList_
    . S.map (either formatStack formatMStack)
    $ listStacks
  where
    header =
      [ "NAME"
      , "STATUS"
      , "CREATION_TIME"
      , "LAST_UPDATE_TIME"
      ]

    formatStack Stack{..} =
      [ pretty _stackName
      , "NOT_MATERIALIZED"
      , "-"
      , "-"
      ]

    formatMStack stack =
      [ pretty . view CF.staStackName $ stack
      , formatStackStatus . view CF.staStackStatus $ stack
      , pretty . view CF.staCreationTime $ stack
      , maybe "-" pretty . view CF.staLastUpdatedTime $ stack
      ]

createDescribeStackReq :: StackName -> CF.DescribeStacks
createDescribeStackReq = flip (CF.dStackName ?~) CF.describeStacks . unStackName

describeChangeSetReq :: ChangeSetId -> CF.DescribeChangeSet
describeChangeSetReq csId = CF.describeChangeSet . unChangeSetId $ csId

uploadResources :: (AWSConstraint' m, PrettyPrint m) => Stack -> Path Abs Dir -> m ()
uploadResources stack resourceDir = withBlock "Uploading resources" $ do
    mapM_ uploadResource' (listResourceIds stack)
    reportSuccess "Upload complete"
  where
    uploadResource' rid = do
      putDocB $ pretty rid <> "... "
      uploadResource rid resourceDir 
      putTextLn "DONE"

printChanges :: (MonadIO m, PrettyPrint m) => CF.DescribeChangeSetResponse -> m ()
printChanges = putDocBLn . listChanges
  where
    listChanges
      = tabulate
      . mkTable
      . cons header
      . fmap formatChange
      . toListOf extractResourceChanges

    extractResourceChanges
      = CF.dcscrsChanges
      . traverse
      . CF.cResourceChange
      . _Just

    header =
      [ "ACTION"
      , "RESOURCE_ID"
      , "TYPE"
      ]

    formatChange change = fmap (fromMaybe "-") $
      [ fmap pretty . view CF.rcAction $ change
      , fmap pretty . view CF.rcLogicalResourceId $ change
      , fmap pretty . view CF.rcResourceType $ change
      ]

formatStackStatus :: CF.StackStatus -> Doc AnsiStyle
formatStackStatus status = case status of
  CF.SSCreateComplete -> annotate (color Green) "CREATE_COMPLETE"
  CF.SSCreateFailed -> annotate (color Red) "CREATE_FAILED"
  CF.SSCreateInProgress -> "CREATE_IN_PROGRESS"
  CF.SSDeleteComplete -> annotate (color Green) "DELETE_COMPLETE"
  CF.SSDeleteFailed -> annotate (color Red) "DELETE_FAILED"
  CF.SSDeleteInProgress -> "DELETE_IN_PROGRESS"
  CF.SSReviewInProgress -> "REVIEW_IN_PROGRESS"
  CF.SSRollbackComplete -> annotate (color Yellow) "ROLLBACK_COMPLETE"
  CF.SSRollbackFailed -> annotate (color Red) "ROLLBACK_FAILED"
  CF.SSRollbackInProgress -> "ROLLBACK_IN_PROGRESS"
  CF.SSUpdateComplete -> annotate (color Green) "UPDATE_COMPLETE"
  CF.SSUpdateCompleteCleanupInProgress -> "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"
  CF.SSUpdateInProgress -> "UPDATE_IN_PROGRESS"
  CF.SSUpdateRollbackComplete -> annotate (color Yellow) "UPDATE_ROLLBACK_COMPLETE"
  CF.SSUpdateRollbackCompleteCleanupInProgress -> annotate (color Yellow) "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
  CF.SSUpdateRollbackFailed -> annotate (color Red) "UPDATE_ROLLBACK_FAILED"
  CF.SSUpdateRollbackInProgress -> "UPDATE_ROLLBACK_IN_PROGRESS"

instance Pretty CF.ChangeAction where
  pretty CF.Add    = "ADD"
  pretty CF.Modify = "MODIFY"
  pretty CF.Remove = "REMOVE"

instance Pretty UTCTime where
  pretty = pretty . formatTime defaultTimeLocale (iso8601DateFormat (Just "%H:%M:%S"))
