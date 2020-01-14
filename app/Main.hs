{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Conduit

import           Control.Exception          (Exception, handle, IOException)
import           Control.Exception.Lens     (handling)
import           Control.Lens.Getter        (view)
import           Control.Lens.Setter        ((.~))
import           Control.Monad              ((=<<), join)
import           Control.Monad.Catch        (handleJust, MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Reader

import           Data.Functor               ((<&>))
import           Data.Text                  (pack, Text)
import           Data.Text.IO               (putStrLn)
import qualified Data.Text as T

import           Dhall

import           LaunchPad.Deploy
import           LaunchPad.Type
import           LaunchPad.Type.Dhall

import           Network.AWS.CloudFormation (StackStatus (..))

import           Options.Applicative

import           Path
import           Path.IO

import           Prelude                    hiding (putStrLn)

import           System.Exit

main :: IO ()
main = reportError . join . execParser $ info parser infoMods
  where
    infoMods = fullDesc <> header "launchpad 0.1.1 - Automate deployment of nested stacks"

parser :: Parser (IO ())
parser = subparser deployCmd <**> helper

deployCmd :: Mod CommandFields (IO ())
deployCmd = command "deploy" $ info parser infoMods
  where
    parser = run
      <$>  confFileOpt
      <*>  stackNameArg
      <*>  templateDirArg
      <**> helper

    infoMods = progDesc $ "Deploy given stack with name STACK_NAME "
      <> "as specified in CONF_FILE. Template identifiers are resolved "
      <> "within the given directory TEMPLATE_DIR."

    run confFile stackName templateDir = do
      conf <- join $ readConfig <$> resolveDir' templateDir <*> resolveFile' confFile
      runResourceT . runAWST conf $ do
        stackId <- deployStack stackName
        liftIO $ putStrLn $ "Tracking status of stack " <> unStackId stackId
        trackStackStatus stackId

confFileOpt :: Parser FilePath
confFileOpt = strOption $
     short   'c'
  <> long    "conf"
  <> metavar "CONF_FILE"
  <> help    "Dhall configuration file containing the deployment configuration for each stack"

stackNameArg :: Parser Text
stackNameArg = strArgument $
     metavar "STACK_NAME"
  <> help    "Name of the stack to be deployed"

templateDirArg :: Parser FilePath
templateDirArg = strArgument $
     metavar "TEMPLATE_DIR"
  <> help    "Directory where template files are located"  

readConfig :: Path Abs Dir -> Path Abs File -> IO Config
readConfig templateDir confFile = do
  env <- newEnv Discover <&> envRegion .~ Frankfurt
  DhallConfig {..} <- readDhallConfig confFile
  return Config { _templateDir = templateDir, _env = env, .. }

readDhallConfig :: Path Abs File -> IO DhallConfig
readDhallConfig = inputFile (autoWith interpretOptions) . toFilePath
  where
    interpretOptions = defaultInterpretOptions
      { fieldModifier = T.dropWhile (== '_')
      , singletonConstructors = Bare
      }

reportError :: IO () -> IO ()
reportError
    = handle reportIOError
    . handle reportAWSError
    . handling _ServiceError reportServiceError
  where
    reportServiceError error = putStrLn $
           "ERROR "
        <> "ServiceError "
        <> extractErrorCode error
        <> ": "
        <> extractErrorMessage error
      where
        extractErrorCode = (\(ErrorCode c) -> c) . view serviceCode
        extractErrorMessage = foldMap (\(ErrorMessage m) -> m) . view serviceMessage

    reportIOError :: IOException -> IO ()
    reportIOError = reportError

    reportAWSError :: Error -> IO ()
    reportAWSError = reportError

    reportError :: Show e => e -> IO ()
    reportError = putStrLn . ("ERROR " <>) . pack . show