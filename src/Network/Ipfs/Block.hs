{-# LANGUAGE RecordWildCards #-}

-- |Operations for interacting with block operations.
module Network.Ipfs.Block
  (
    -- * Common types
    BlockResponse(blockKey, blockSize)
    -- * Block retrieval
  , OpGetBlock(..)
    -- * Block creation
  , PutBlockOptions(putBlockMhType, putBlockMhLen)
  , defaultPutBlockOptions
  , OpPutBlock(..)
    -- * Block removal
  , RemoveBlockOptions(removeForce, removeQuiet)
  , defaultRemoveBlockOptions
  , OpRemoveBlock(..)
  , RemoveBlockResponse(blockHash, blockError)
    -- * Block statistics
  , OpBlockStat(..)
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
    in IpfsHttpInfo GetText ["block", "get"] query

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

-- |The response for operations that return geneirc information blocks. 
data BlockResponse = BlockResponse
  { blockKey :: T.Text
  , blockSize :: Int
  } deriving (Show, Generic)

instance FromJSON BlockResponse where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpPutBlock where
  type IpfsResponse OpPutBlock = BlockResponse
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
  
data OpBlockStat = OpBlockStat B.ByteString deriving (Show)

instance IpfsOperation OpBlockStat where
  type IpfsResponse OpBlockStat = BlockResponse
  toHttpInfo (OpBlockStat hash) = IpfsHttpInfo Get ["block", "stat"] $ singletonQuery "arg" hash
