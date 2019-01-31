{-# LANGUAGE RecordWildCards #-}

-- |Operations about the IPNS system.
module Network.Ipfs.Name
  (
    IpnsEntry(..)
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
