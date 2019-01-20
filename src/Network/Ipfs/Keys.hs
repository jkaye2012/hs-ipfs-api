{-# LANGUAGE RecordWildCards #-}

-- | Operations about IPFS PeerID keys.
-- Keys are used for many other IPFS api operations.
module Network.Ipfs.Keys
  (
    -- * Key response types
    IpfsKey(..)
  , IpfsKeyList(..)
  , RenamedIpfsKey(..)
    -- * Key operations
  , KeyGenOptions(..)
  , OpKeyGen(..)
  , OpKeyList(..)
  , OpKeyRename(..)
  , OpKeyRemove(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Default

import Network.Ipfs.Core
import Network.Ipfs.Types

-- |An individual key.
data IpfsKey = IpfsKey
  { keyName :: T.Text
  , keyId :: T.Text
  } deriving (Show, Generic)

instance FromJSON IpfsKey where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |A list of keys.
data IpfsKeyList = IpfsKeyList
  { listKeys :: [IpfsKey]
  } deriving (Show, Generic)

instance FromJSON IpfsKeyList where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |A key that has been renamed to a new value.
data RenamedIpfsKey = RenamedIpfsKey
  { renamedWas :: T.Text
  , renamedNow :: T.Text
  , renamedId :: T.Text
  , renamedOverwrite :: Bool
  } deriving (Show, Generic)

instance FromJSON RenamedIpfsKey where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |Options for the 'OpKeyGen' operation.
data KeyGenOptions = KeyGenOptions
  { keyGenType :: IpfsKeyType
  , keyGenSize :: Int
  } deriving (Show, Generic)

instance Default KeyGenOptions where
  def = KeyGenOptions { keyGenType = def, keyGenSize = 4096 }

-- |https://docs.ipfs.io/reference/api/http/#api-v0-key-gen
data OpKeyGen = OpKeyGen B.ByteString KeyGenOptions
  deriving (Show)

-- |https://docs.ipfs.io/reference/api/http/#api-v0-key-list
data OpKeyList = OpKeyList
  deriving (Show)

-- |https://docs.ipfs.io/reference/api/http/#api-v0-key-rename
data OpKeyRename = OpKeyRename B.ByteString B.ByteString (Maybe Bool)
  deriving (Show)

-- |https://docs.ipfs.io/reference/api/http/#api-v0-key-rm
data OpKeyRemove = OpKeyRemove B.ByteString
  deriving (Show)

instance IpfsOperation OpKeyGen where
  type IpfsResponse OpKeyGen = IpfsKey
  toHttpInfo (OpKeyGen name KeyGenOptions{..}) = IpfsHttpInfo Get ["key", "gen"] query
    where query = newQuery [ toQueryItem "arg" name
                           , toQueryItem "type" keyGenType
                           , toQueryItem "size" keyGenSize
                           ]

instance IpfsOperation OpKeyList where
  type IpfsResponse OpKeyList = IpfsKeyList
  toHttpInfo _ = IpfsHttpInfo Get ["key", "list"] emptyQuery

instance IpfsOperation OpKeyRename where
  type IpfsResponse OpKeyRename = RenamedIpfsKey
  toHttpInfo (OpKeyRename from to force) = IpfsHttpInfo Get ["key", "rename"] query
    where query = newQuery [ toQueryItem "arg" from
                           , toQueryItem "arg" to
                           , toQueryItem "force" force
                           ]

instance IpfsOperation OpKeyRemove where
  type IpfsResponse OpKeyRemove = IpfsKeyList
  toHttpInfo (OpKeyRemove name) = IpfsHttpInfo Get ["key", "rm"] query
    where query = singletonQuery "arg" name
