{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |Operations for interacting with block operations.
module Network.Ipfs.Block
  (
    -- * Block retrieval
    OpGetBlock(..)
    -- * Block creation
  , PutBlockResponse(..)
  , PutBlockOptions(putBlockMhType, putBlockMhLen)
  , defaultPutBlockOptions
  , OpPutBlock(..)
  ) where

import Data.Aeson.Types (Value)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Word

import Network.Ipfs.Core

-- |https://docs.ipfs.io/reference/api/http/#api-v0-block-get
data OpGetBlock = OpGetBlock B.ByteString
  deriving Show

instance IpfsOperation OpGetBlock where
  type IpfsResponse OpGetBlock = T.Text
  toHttpInfo (OpGetBlock hash) =
    let query = newQuery [IpfsQueryItem ("arg", Just hash)]
    in IpfsHttpInfo Get ["block", "get"] query

-- |Models options for the 'OpPutBlock' operation.'
data PutBlockOptions = PutBlockOptions
  { putBlockMhType :: B.ByteString
  , putBlockMhLen :: Int
  } deriving (Show)

-- |Defaults matching the API defaults for putting blocks.
defaultPutBlockOptions :: PutBlockOptions
defaultPutBlockOptions = PutBlockOptions { putBlockMhType = "sha2-256"
                                         , putBlockMhLen = -1
                                         }

-- |https://docs.ipfs.io/reference/api/http/#api-v0-block-put
data OpPutBlock = OpPutBlock PutBlockOptions Part
  deriving (Show)

-- |The response for the 'OpPutBlock' operation.
data PutBlockResponse = PutBlockResponse
  { blockKey :: T.Text
  , blockSize :: Int
  } deriving (Show, Generic)

instance FromJSON PutBlockResponse where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpPutBlock where
  type IpfsResponse OpPutBlock = PutBlockResponse
  toHttpInfo (OpPutBlock PutBlockOptions{..} file) =
    let query = newQuery [ toQueryItem "mhtype" putBlockMhType
                         , toQueryItem "mhlen" putBlockMhLen
                         ]
    in IpfsHttpInfo (Post file) ["block", "put"] query

data RemoveBlockOptions = RemoveBlockOptions
  { removeForce :: Bool
  , removeQuiet :: Bool
  } deriving (Show)

defaultRemoveBlockOptions :: RemoveBlockOptions
defaultRemoveBlockOptions = RemoveBlockOptions { removeForce = False
                                               , removeQuiet = False
                                               }

data OpRemoveBlock = OpRemoveBlock RemoveBlockOptions B.ByteString
  deriving (Show)

data RemoveBlockResponse = RemoveBlockResponse { blockHash :: T.Text
                                               , blockError :: T.Text
                                               } deriving (Show, Generic)

instance FromJSON RemoveBlockResponse where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpRemoveBlock where
  type IpfsResponse OpRemoveBlock = RemoveBlockResponse
  toHttpInfo (OpRemoveBlock RemoveBlockOptions{..} hash) =
    let query = newQuery [ toQueryItem "arg" hash
                         , toQueryItem "force" removeForce
                         , toQueryItem "quiet" removeQuiet ]
    in IpfsHttpInfo Get ["block", "remove"] query
  
