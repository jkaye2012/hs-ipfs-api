{-# LANGUAGE RecordWildCards #-}

-- |Operations about the IPFS distrbuted data structures.
-- Mainly these are:
-- 1. The Markel DAG
-- 2. The distributed hash table (dht)
module Network.Ipfs.Distributed
  (
    OpGetDagNode(..)
  , OpPutDagNode(..)
  , PutDagNodeOptions(..)
  , OpFindPeer(..)
  , OpFindProviders(..)
  , FindProvidersOptions(..)
  , OpProvideDht(..)
  , ProvideDhtOptions(..)
  , OpPutDht(..)
  , OpGetDht(..)
  , OpQueryDht(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Default(Default)

import Network.Ipfs.Core

-- |https://docs.ipfs.io/reference/api/http/#api-v0-dag-get
data OpGetDagNode = OpGetDagNode B.ByteString
  deriving (Show)

instance IpfsOperation OpGetDagNode where
  type IpfsResponse OpGetDagNode = T.Text
  toHttpInfo (OpGetDagNode node) = IpfsHttpInfo GetText ["dag", "get"] query
    where
      query = newQuery [ toQueryItem "arg" node ]

-- |Options for the 'OpPutDagNode' operation.
-- TODO: Format, encoding, and hash could be strong types instead of string
data PutDagNodeOptions = PutDagNodeOptions
  { dagNodeFormat :: Maybe B.ByteString
  , dagNodeEncoding :: Maybe B.ByteString
  , dagNodePin :: Maybe Bool
  , dagNodeHash :: Maybe B.ByteString
  } deriving (Show, Generic)

instance Default PutDagNodeOptions

-- |https://docs.ipfs.io/reference/api/http/#api-v0-dag-put
data OpPutDagNode = OpPutDagNode Part PutDagNodeOptions
  deriving (Show)

instance IpfsOperation OpPutDagNode where
  type IpfsResponse OpPutDagNode = T.Text
  toHttpInfo (OpPutDagNode node PutDagNodeOptions{..}) = IpfsHttpInfo (PostText node) ["dag", "put"] query
    where
      query = newQuery [ toQueryItem "format" dagNodeFormat
                       , toQueryItem "input-enc" dagNodeEncoding
                       , toQueryItem "pin" dagNodePin
                       , toQueryItem "hash" dagNodeHash
                       ]

-- |https://docs.ipfs.io/reference/api/http/#api-v0-dag-resolve
data OpResolveDagBlock = OpResolveDagBlock B.ByteString
  deriving (Show)

-- |The response type for the 'OpResolveDagBlock' operation.'
data DagBlock = DagBlock
  { blockCid :: T.Text
  , blockRemPath :: T.Text
  } deriving (Show, Generic)

instance FromJSON DagBlock where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpResolveDagBlock where
  type IpfsResponse OpResolveDagBlock = DagBlock
  toHttpInfo (OpResolveDagBlock path) = IpfsHttpInfo Get ["dag", "resolve"] query
    where
      query = newQuery [ toQueryItem "arg" path ]

-- |https://docs.ipfs.io/reference/api/http/#api-v0-dht-findpeer
data OpFindPeer = OpFindPeer B.ByteString
  deriving (Show)

data PeerResponse = PeerResponse
  { responseID :: T.Text
  , responseAddrs :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON PeerResponse where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |The response type for all DHT operations.
data DhtPeer = DhtPeer
  { peerID :: T.Text
  , peerType :: Int
  , peerResponses :: [PeerResponse]
  , peerExtra :: T.Text
  } deriving (Show, Generic)

instance FromJSON DhtPeer where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpFindPeer where
  type IpfsResponse OpFindPeer = DhtPeer
  toHttpInfo (OpFindPeer peerId) = IpfsHttpInfo Get ["dht", "findpeer"] query
    where
      query = newQuery [ toQueryItem "arg" peerId ]

-- |Options for the 'OpFindProviders' operation.
data FindProvidersOptions = FindProvidersOptions
  { providersNum :: Maybe Int
  } deriving (Show, Generic)

instance Default FindProvidersOptions

-- |https://docs.ipfs.io/reference/api/http/#api-v0-dht-findprovs
data OpFindProviders = OpFindProviders B.ByteString FindProvidersOptions
  deriving (Show)

instance IpfsOperation OpFindProviders where
  type IpfsResponse OpFindProviders = DhtPeer
  toHttpInfo (OpFindProviders key FindProvidersOptions{..}) = IpfsHttpInfo Get ["dht", "findprovs"] query
    where
      query = newQuery [ toQueryItem "arg" key
                       , toQueryItem "num-providers" providersNum
                       ]

-- |https://docs.ipfs.io/reference/api/http/#api-v0-dht-get
data OpGetDht = OpGetDht B.ByteString
  deriving (Show)

instance IpfsOperation OpGetDht where
  type IpfsResponse OpGetDht = DhtPeer
  toHttpInfo (OpGetDht key) = IpfsHttpInfo Get ["dht", "get"] query
    where
      query = newQuery [ toQueryItem "arg" key ]

-- |Options for the 'OpProvideDht' operation.'
data ProvideDhtOptions = ProvideDhtOptions
  { provideRecursive :: Maybe Bool
  } deriving (Show, Generic)

instance Default ProvideDhtOptions

-- |https://docs.ipfs.io/reference/api/http/#api-v0-dht-provide
data OpProvideDht = OpProvideDht B.ByteString ProvideDhtOptions
  deriving (Show)

instance IpfsOperation OpProvideDht where
  type IpfsResponse OpProvideDht = DhtPeer
  toHttpInfo (OpProvideDht key ProvideDhtOptions{..}) = IpfsHttpInfo Get ["dht", "provide"] query
    where
      query = newQuery [ toQueryItem "arg" key
                       , toQueryItem "recursive" provideRecursive
                       ]

-- |https://docs.ipfs.io/reference/api/http/#api-v0-dht-put
data OpPutDht = OpPutDht B.ByteString B.ByteString
  deriving (Show)

instance IpfsOperation OpPutDht where
  type IpfsResponse OpPutDht = DhtPeer
  toHttpInfo (OpPutDht key value) = IpfsHttpInfo Get ["dht", "put"] query
    where
      query = newQuery [ toQueryItem "arg" key
                       , toQueryItem "arg" value
                       ]
-- |https://docs.ipfs.io/reference/api/http/#api-v0-dht-query
data OpQueryDht = OpQueryDht B.ByteString
  deriving (Show)

instance IpfsOperation OpQueryDht where
  type IpfsResponse OpQueryDht = DhtPeer
  toHttpInfo (OpQueryDht peerId) = IpfsHttpInfo Get ["dht", "query"] query
    where
      query = newQuery [ toQueryItem "arg" peerId ]
