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
