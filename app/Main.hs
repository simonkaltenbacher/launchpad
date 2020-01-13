{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Conduit

import           Control.Exception          (Exception, SomeException (..))
import           Control.Exception.Lens     (handling)
import           Control.Lens.Getter        (view)
import           Control.Lens.Setter        ((.~))
import           Control.Monad              ((=<<), join)
import           Control.Monad.Catch        (handleAll, MonadCatch)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.AWS
import           Control.Monad.Trans.Reader

import           Data.Bifunctor             (first)
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

import           Prelude                    hiding (putStrLn)

main :: IO ()
main = reportError . join $ execParser opts

opts :: ParserInfo (IO ())
opts = info (run <$> confFileOpt <*> stackNameArg <*> templateDirArg <**> helper) idm
  where
    run confFile stackName templateDir = do
      conf <- readConfig templateDir confFile
      runResourceT . runAWST conf $ do
        stackId <- deployStack stackName
        liftIO $ putStrLn $ "Tracking status of stack " <> unStackId stackId
        trackStackStatus stackId

confFileOpt :: Parser (Path Abs File)
confFileOpt = option (eitherReader $ first show . parseAbsFile) $
     short 'c'
  <> long "conf"
  <> metavar "CONF_FILE"

stackNameArg :: Parser Text
stackNameArg = strArgument (metavar "STACK_NAME")

templateDirArg :: Parser (Path Abs Dir)
templateDirArg = argument (eitherReader $ first show . parseAbsDir) (metavar "TEMPLATE_DIR")

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
reportError = handleAll reportOther . handling _ServiceError reportServiceError
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

    reportOther = putStrLn . ("ERROR " <>) . pack . show