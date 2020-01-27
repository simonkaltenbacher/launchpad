{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main where

import Conduit

import Control.Exception        (IOException)
import Control.Exception.Lens   (handling)
import Control.Monad            ((=<<), join)
import Control.Monad.Catch      (handle, MonadCatch)
import Control.Monad.Reader
import Control.Monad.Trans.AWS

import LaunchPad.CloudFormation
import LaunchPad.Config
import LaunchPad.Exception
import LaunchPad.PrettyPrint

import Options.Applicative

import Path.IO

import Relude


main :: IO ()
main = join . liftIO . execParser $ info parser infoMods
  where
    infoMods = fullDesc <> header "launchpad 0.2.0-alpha - Automate deployment of nested stacks"

parser :: Parser (IO ())
parser = subparser deployCmd <**> helper

deployCmd :: Mod CommandFields (IO ())
deployCmd = command "deploy" $ info parser infoMods
  where
    parser = run
      <$>  confFileOpt
      <*>  disableRollbackSwitch
      <*>  stackNameArg
      <*>  resourceDirArg
      <**> helper

    infoMods = progDesc $ "Deploy given stack with name STACK_NAME "
      <> "as specified in CONF_FILE. Template identifiers are resolved "
      <> "within the given directory RESOURCE_DIR."

    run confFile disableRollback stackName resourceDir = reportError $ do
      conf <- join $ readConfig <$> resolveDir' resourceDir <*> resolveFile' confFile
      runResourceT . runAWST conf . runPretty initPretty $ do
        stack <- findStack stackName =<< asks _stacks
        void $ deployStack disableRollback stack

confFileOpt :: Parser FilePath
confFileOpt = strOption $
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
  <> help    "Name of the stack to be deployed"

resourceDirArg :: Parser FilePath
resourceDirArg = strArgument $
     metavar "RESOURCE_DIR"
  <> help    "Directory where resources such as templates and scripts are located"  

reportError :: forall m. (MonadCatch m, MonadIO m) => m () -> m ()
reportError
    = handle printIOException
    . handle printAWSError
    . handling _ServiceError printServiceError
    . handle printLaunchPadException
  where
    printServiceError = printErrorPretty

    printIOException :: IOException -> m ()
    printIOException = printErrorPretty

    printAWSError :: Error -> m ()
    printAWSError = printErrorPretty

    printLaunchPadException :: SomeLaunchPadException -> m ()
    printLaunchPadException = printErrorPretty

printErrorPretty :: (MonadIO m, Pretty e) => e -> m ()
printErrorPretty = putTextLn . renderDoc . pretty