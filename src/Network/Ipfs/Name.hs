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
    -- * Reading pubsub state
  , PubsubState(..)
  , OpPubsubState(..)
    -- * Inspecting pubsub subscriptions
  , PubsubSubscriptions(..)
  , OpPubsubSubscriptions(..)
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

-- |The response type for the 'OpPubsubState' operation.'
data PubsubState = PubsubState
  { pubsubEnabled :: Bool
  } deriving (Show, Generic)

instance FromJSON PubsubState where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |https://docs.ipfs.io/reference/api/http/#api-v0-name-pubsub-state
data OpPubsubState = OpPubsubState
  deriving (Show)

instance IpfsOperation OpPubsubState where
  type IpfsResponse OpPubsubState = PubsubState
  toHttpInfo _ = IpfsHttpInfo Get ["name", "pubsub", "state"] emptyQuery

-- |The response type for the 'OpPubsubSubscriptions' operation.
data PubsubSubscriptions = PubsubSubscriptions
  { subscriptionStrings :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON PubsubSubscriptions where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |https://docs.ipfs.io/reference/api/http/#api-v0-name-pubsub-subs
data OpPubsubSubscriptions = OpPubsubSubscriptions
  deriving (Show)

instance IpfsOperation OpPubsubSubscriptions where
  type IpfsResponse OpPubsubSubscriptions = PubsubSubscriptions
  toHttpInfo _ = IpfsHttpInfo Get ["name", "pubsub", "subs"] emptyQuery

-- |Options for the 'OpPubsubResolve' operation.
data NameResolutionOptions = NameResolutionOptions
  { nameresName :: Maybe B.ByteString
  , nameresRecursive :: Maybe Bool
  , nameresNocache :: Maybe Bool
  , nameresRecordCount :: Maybe Word
  , namesresTimeout :: Maybe B.ByteString
  } deriving (Show, Generic)

instance Default NameResolutionOptions

-- |The response type for the 'OpPubsubResolve' operation.
data NameResolution = NameResolution
  { resolvedPaths :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON NameResolution where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |https://docs.ipfs.io/reference/api/http/#api-v0-name-resolve
data OpPubsubResolve = OpPubsubResolve NameResolutionOptions
  deriving (Show)

instance IpfsOperation OpPubsubResolve where
  type IpfsResponse OpPubsubResolve = NameResolution
  toHttpInfo (OpPubsubResolve NameResolutionOptions{..}) = IpfsHttpInfo Get ["name", "resolve"] query
    where query = newQuery [ toQueryItem "arg" nameresName
                           , toQueryItem "recursive" nameresRecursive
                           , toQueryItem "nocache" nameresNocache
                           , toQueryItem "dht-record-count" nameresRecordCount
                           , toQueryItem "dht-timeout" namesresTimeout
                           ]
