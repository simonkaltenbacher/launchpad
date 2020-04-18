{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LaunchPad.CloudFormation
  ( AWSConstraint'
  , runCreateStack
  , runDeleteStack
  , runDeployStack
  , runDiagnoseStack
  , runListStacks
  , findStack
  )
  where

import           Control.Lens
import           Control.Monad.Catch               (throwM)
import           Control.Monad.Reader

import           Data.Time.Format

import           LaunchPad.AWS
import           LaunchPad.CloudFormation.Internal
import           LaunchPad.Exception
import           LaunchPad.PrettyPrint
import           LaunchPad.Wait

import qualified Network.AWS.CloudFormation as CF
import           Network.AWS.Data.Time             (UTCTime)

import           Path

import           Relude.Custom

import qualified Streaming.Prelude as S


runCreateStack :: (AWSConstraint' m, PrettyPrint m) => Bool -> StackName -> Path Abs Dir -> m ()
runCreateStack disableRollback stackName resourceDir  = do
  (stack @ Stack{..}) <- findStack stackName =<< asks _stacks
  uploadResources stack resourceDir
  withBlock ("Create stack " <> pretty _stackName) $ do
    createStack disableRollback stack
    await (stackCreateOrUpdateComplete "Creating stack") (createDescribeStackReq (fromStackName _stackName))
    reportSuccess "Stack creation complete"

runDeleteStack :: (AWSConstraint' m, PrettyPrint m) => StackName -> m ()
runDeleteStack stackName = do
    (stack @ Stack{..}) <- findStack stackName =<< asks _stacks
    withBlock ("Delete stack " <> pretty _stackName) $
      ifM (stackExists (fromStackName _stackName)) deleteStack' err
  where
    deleteStack' = do
      deleteStack (fromStackName stackName)
      await deleteStackComplete (createDescribeStackReq (fromStackName stackName))
      reportSuccess "Stack deletion complete"

    err = throwM . StackNotFoundException . renderDoc $ "Stack " <> pretty stackName <> " does not exist"

runDeployStack :: (AWSConstraint' m, PrettyPrint m) => StackName -> Path Abs Dir -> m ()
runDeployStack stackName resourceDir = do
    (stack @ Stack{..}) <- findStack stackName =<< asks _stacks
    uploadResources stack resourceDir 
    withBlock ("Deploy stack " <> pretty _stackName) $ do
      resBucketName <- asks _resourceBucketName
      csType <- bool CF.CSTCreate CF.CSTUpdate <$> stackExists (fromStackName _stackName)
      csId <- createChangeSet csName csType stack
      csCreateComplete <- await changeSetCreateComplete (describeChangeSetReq (fromChangeSetId csId))
      reportSuccess "Created change set"
      withBlock "The following changes will be applied:" (printChanges csCreateComplete)
      whenM (getConfirmation 3 parser "Do you want to continue? [y/n]: ") $ do
        executeChangeSet (fromChangeSetId csId)
        await (stackCreateOrUpdateComplete "Deploying stack") (createDescribeStackReq (fromStackName _stackName))
        reportSuccess "Stack deployment complete"
  where
    csName = ChangeSetName (unStackName stackName)

    parser "y" = Just True
    parser "n" = Just False
    parser _   = Nothing

runDiagnoseStack :: (AWSConstraint' m, PrettyPrint m) => StackName -> m ()
runDiagnoseStack stackName = do
    lastStart <- findLastExecutionStart (fromStackName stackName)
    ifM (existError lastStart stackName)
      (withBlock "Showing errors of last stack execution:" $ putErrors lastStart stackName)
      (putDocBLn "No errors for last stack execution found!")
  where
    existError lastStart
      = fmap (not . null)
      . S.head_
      . diagnoseFailure lastStart
      . fromStackName

    putErrors lastStart
      = S.mapM_ (putDocBLn . (line <>) . tabulate . mkTable . formatStackEvent)
      . S.map (over (CF.seResourceStatusReason . _Just) shortenArn)
      . diagnoseFailure lastStart
      . fromStackName

    header =
      [ "TIME"
      , "STACK_NAME"
      , "STACK_ID"
      , "LOGICAL_RESOURCE_ID"
      , "PHYSICAL_RESOURCE_ID"
      , "RESOURCE_TYPE"
      , "STATUS"
      , "STATUS_REASON"
      ]

    formatStackEvent event = zipWith ((. pure) . cons) header
      [ pretty . view CF.seTimestamp $ event
      , pretty . view CF.seStackName $ event
      , pretty . view CF.seStackId $ event
      , maybe "-" pretty . view CF.seLogicalResourceId $ event
      , maybe "-" pretty . view CF.sePhysicalResourceId $ event
      , maybe "-" pretty . view CF.seResourceType $ event
      , maybe "-" formatResourceStatus . view CF.seResourceStatus $ event
      , maybe "-" pretty . view CF.seResourceStatusReason $ event
      ]
      
runListStacks :: (AWSConstraint' m, PrettyPrint m) => m ()
runListStacks
    = liftD (putDocBLn . tabulate . mkTable . cons header)
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

createDescribeStackReq :: StackIdLike -> CF.DescribeStacks
createDescribeStackReq = flip (CF.dStackName ?~) CF.describeStacks . unStackIdLike

describeChangeSetReq :: ChangeSetIdLike -> CF.DescribeChangeSet
describeChangeSetReq = CF.describeChangeSet . unChangeSetIdLike

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

formatResourceStatus :: CF.ResourceStatus -> Doc AnsiStyle
formatResourceStatus status = case status of
  CF.CreateComplete -> annotate (color Green) "CREATE_COMPLETE"
  CF.CreateFailed -> annotate (color Red) "CREATE_FAILED"
  CF.CreateInProgress -> "CREATE_IN_PROGRESS"
  CF.DeleteComplete -> annotate (color Green) "DELETE_COMPLETE"
  CF.DeleteFailed -> annotate (color Red) "DELETE_FAILED"
  CF.DeleteInProgress -> "DELETE_IN_PROGRESS"
  CF.DeleteSkipped -> "DELETE_IN_SKIPPED"
  CF.UpdateComplete -> annotate (color Green) "UPDATE_COMPLETE"
  CF.UpdateFailed -> annotate (color Red) "UPDATE_FAILED"
  CF.UpdateInProgress -> "UPDATE_IN_PROGRESS"
  CF.UpdateRollbackComplete -> "UPDATE_ROLLBACK_COMPLETE"
  CF.UpdateRollbackCompleteCleanupInProgress -> "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
  CF.UpdateRollbackInProgress -> "UPDATE_ROLLBACK_IN_PROGRESS"

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
