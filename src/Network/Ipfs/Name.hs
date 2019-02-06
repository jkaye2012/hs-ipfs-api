{-# LANGUAGE RecordWildCards #-}

-- |Operations about the IPNS system.
module Network.Ipfs.Name
  (
    -- * Common types
    IpnsEntry(..)
    -- * Publishing new names
  , IpnsPublishOptions(..)
  , OpIpnsPublish(..)
    -- * Cancelling existing subscriptions
  , PubsubCancelResponse(..)
  , OpPubsubCancel(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Default(Default)

import Network.Ipfs.Core

data IpnsEntry = IpnsEntry
  { ipnsName :: T.Text
  , ipnsValue :: T.Text
  } deriving (Show, Generic)

instance FromJSON IpnsEntry where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

data IpnsPublishOptions = IpnsPublishOptions
  { publishResolve :: Maybe Bool
  , publishLifetime :: Maybe B.ByteString
  , publishKey :: Maybe B.ByteString
  } deriving (Show, Generic)

instance Default IpnsPublishOptions

data OpIpnsPublish = OpIpnsPublish B.ByteString IpnsPublishOptions
  deriving (Show)

instance IpfsOperation OpIpnsPublish where
  type IpfsResponse OpIpnsPublish = IpnsEntry
  toHttpInfo (OpIpnsPublish path IpnsPublishOptions{..}) = IpfsHttpInfo Get ["name", "publish"] query
    where query = newQuery [ toQueryItem "arg" path
                           , toQueryItem "resolve" publishResolve
                           , toQueryItem "lifetime" publishLifetime
                           , toQueryItem "key" publishKey
                           ]

-- |The response type for te 'OpPubsubCancel' operation.
data PubsubCancelResponse = PubsubCancelResponse
  { pubsubCanceled :: Bool
  } deriving (Show, Generic)

instance FromJSON PubsubCancelResponse where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |https://docs.ipfs.io/reference/api/http/#api-v0-name-pubsub-cancel
data OpPubsubCancel = OpPubsubCancel B.ByteString
  deriving (Show)

instance IpfsOperation OpPubsubCancel where
  type IpfsResponse OpPubsubCancel = PubsubCancelResponse
  toHttpInfo (OpPubsubCancel name) = IpfsHttpInfo Get ["name", "pubsub", "cancel"] query
    where query = singletonQuery "arg" name
