{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Conduit

import Control.Exception        (IOException)
import Control.Exception.Lens   (handling)
import Control.Monad.Catch      (handle, MonadCatch)
import Control.Monad.Reader
import Control.Monad.Trans.AWS

import LaunchPad.CloudFormation
import LaunchPad.Config
import LaunchPad.Exception
import LaunchPad.PrettyPrint
import LaunchPad.Version

import Options.Applicative

import Path.IO

import Relude

import System.Environment       (getEnv)


data Mode
  = Create
      { confFile        :: Maybe FilePath
      , disableRollback :: Bool
      , stackName       :: StackName
      , resourceDir     :: FilePath
      }
  | Delete
      { confFile  :: Maybe FilePath
      , stackName :: StackName
      }
  | Deploy
      { confFile    :: Maybe FilePath
      , stackName   :: StackName
      , resourceDir :: FilePath
      }
  | List
      { confFile :: Maybe FilePath
      }
  | Version

main :: IO ()
main = handleError $ run =<< execParser (info parser infoMods)
  where
    infoMods = fullDesc <> header
      (  "launchpad "
      <> toString launchPadVersionString
      <> " - Simplify deployment of nested stacks"
      )

    parser = subparser (createCmd <> deleteCmd <> deployCmd <> listCmd <> versionCmd) <**> helper
   
    run Create{..} = runWithConf confFile $ runCreateStack disableRollback stackName =<< resolveDir' resourceDir
    run Delete{..} = runWithConf confFile $ runDeleteStack stackName
    run Deploy{..} = runWithConf confFile $ runDeployStack stackName =<< resolveDir' resourceDir
    run List{..}   = runWithConf confFile $ runListStacks
    run Version    = putTextLn launchPadVersionString

runWithConf
  :: MonadCatch m
  => MonadIO m
  => MonadUnliftIO m
  => Maybe FilePath
  -> StateT PrettyState (AWST' Config (ResourceT m)) ()
  -> m ()
runWithConf confFile m = do
  conf <- readConfig =<< resolveFile' =<< maybe (liftIO . getEnv $ "LAUNCHPAD_CONF") pure confFile
  runResourceT . runAWST conf . runPretty initPretty $ m

createCmd :: Mod CommandFields Mode
createCmd = command "create" $ info parser infoMods
  where
    parser = Create
      <$>  confFileOpt
      <*>  disableRollbackSwitch
      <*>  stackNameArg
      <*>  resourceDirArg
      <**> helper

    infoMods = progDesc "Create given stack" <> createHeader

    createHeader = header $ "Create given stack with name STACK_NAME "
      <> "as specified in CONF_FILE. Template identifiers are resolved "
      <> "within the given directory RESOURCE_DIR."

deleteCmd :: Mod CommandFields Mode
deleteCmd = command "delete" $ info parser infoMods
  where
    parser = Delete
      <$>  confFileOpt
      <*>  stackNameArg
      <**> helper

    infoMods = progDesc "Delete given stack" <> createHeader

    createHeader = header $ "Delete given stack with name STACK_NAME as specified in CONF_FILE."

deployCmd :: Mod CommandFields Mode
deployCmd = command "deploy" $ info parser infoMods
  where
    parser = Deploy
      <$>  confFileOpt
      <*>  stackNameArg
      <*>  resourceDirArg
      <**> helper

    infoMods = progDesc "Deploy given stack" <> deployHeader

    deployHeader = header $ "Deploy given stack with name STACK_NAME "
      <> "as specified in CONF_FILE. Template identifiers are resolved "
      <> "within the given directory RESOURCE_DIR."

listCmd :: Mod CommandFields Mode
listCmd = command "list" $ info parser infoMods
  where
    parser = List <$> confFileOpt <**> helper

    infoMods = progDesc "List stacks"

versionCmd :: Mod CommandFields Mode
versionCmd = command "version" $ info (pure Version) infoMods
  where
    infoMods = progDesc "Show version"

confFileOpt :: Parser (Maybe FilePath)
confFileOpt = optional $ strOption $
     short   'c'
  <> long    "conf"
  <> metavar "CONF_FILE"
  <> help    "Dhall configuration file containing the deployment configuration for each stack"

disableRollbackSwitch :: Parser Bool
disableRollbackSwitch = switch $
     long "disable-rollback"
  <> help "Disable rollback of stack if stack creation fails"

stackNameArg :: Parser StackName
stackNameArg = argument auto $
     metavar "STACK_NAME"
  <> help    "Name of the stack"

resourceDirArg :: Parser FilePath
resourceDirArg = strArgument $
     metavar "RESOURCE_DIR"
  <> help    "Directory where resources such as templates and scripts are located"  

versionSwitch :: Parser Bool
versionSwitch = switch $
     long "version"
  <> help "Show version"

handleError :: forall m. (MonadCatch m, MonadIO m) => m () -> m ()
handleError
    = handle reportIOException
    . handle reportAWSError
    . handling _ServiceError reportServiceError
    . handle reportLaunchPadException
  where
    reportServiceError = reportError . pretty

    reportIOException :: IOException -> m ()
    reportIOException = reportError . pretty

    reportAWSError :: Error -> m ()
    reportAWSError = reportError . pretty

    reportLaunchPadException :: SomeLaunchPadException -> m ()
    reportLaunchPadException = reportError . pretty