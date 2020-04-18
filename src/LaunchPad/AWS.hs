{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module LaunchPad.AWS
  ( Arn (..)
  , ArnResource (..)
  , shortenArn
  )
  where

import Control.Lens

import Data.Attoparsec.Text
import Data.Char               (isAlphaNum)

import Network.AWS.Data        (FromText (..))

import Relude.Custom           hiding (takeWhile)

import Replace.Attoparsec.Text (streamEdit)


data Arn = Arn
  { _partition   :: Text
  , _service     :: Text
  , _region      :: Text
  , _accountId   :: Text
  , _arnResource :: ArnResource
  }
  deriving (Eq, Show)

data ArnResource = ArnResource
  { _resourceType :: Text
  , _resource     :: Text
  , _qualifier    :: Maybe Text
  }
  deriving (Eq, Show)

$(makeLenses ''Arn)
$(makeLenses ''ArnResource)

isAllowedChar :: Char -> Bool
isAllowedChar = notInClass "/: "

pArn :: Parser Arn
pArn = Arn
  <$> (string "arn:" *> takeWhile1 isAllowedChar)
  <*> (char ':' *> takeWhile1 isAllowedChar)
  <*> (char ':' *> takeWhile isAllowedChar)
  <*> (char ':' *> takeWhile isAllowedChar)
  <*> (char ':' *> pArnResource)

pArnResource :: Parser ArnResource
pArnResource = ArnResource
  <$> takeWhile1 isAlphaNum
  <*> ((char '/' <|> char ':') *> takeWhile1 isAllowedChar)
  <*> optional ((char '/' <|> char ':') *> takeWhile1 isAllowedChar)

instance FromText Arn where
  parser = pArn

shortenArn :: Text -> Text
shortenArn = streamEdit pArn (fromMaybe "" . view (arnResource . qualifier))