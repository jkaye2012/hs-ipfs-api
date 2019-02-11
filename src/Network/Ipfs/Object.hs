{-# LANGUAGE RecordWildCards #-}

-- |Operations for interacting with the IPFS object api.
module Network.Ipfs.Object
  (
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Default(Default)

import Network.Ipfs.Core

-- * Common types

-- |Represents links from one IPFS object to another.
-- Links are one-way relationships from a source object to a destination object.
data ObjectLink = ObjectLink
  { linkName :: T.Text
  , linkHash :: T.Text
  , linkSize :: Word
  } deriving (Show, Generic)

instance FromJSON ObjectLink where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |Represents a single IPFS object.
data IpfsObject = IpfsObject
  { objectLinks :: [ObjectLink]
  , objectData :: T.Text
  } deriving (Show, Generic)

instance FromJSON IpfsObject where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |Metadata for a single IPFS object.
-- This is very similar to an 'IpfsObject' except that the object's hash is included
-- instead of its actual data.
data IpfsObjectMetadata = IpfsObjectMetadata
  { metadataLinks :: [ObjectLink]
  , metadataHash :: T.Text
  } deriving (Show, Generic)

instance FromJSON IpfsObjectMetadata where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- * Operation definitions

-- |https://docs.ipfs.io/reference/api/http/#api-v0-object-data
data OpObjectData = OpObjectData B.ByteString
  deriving (Show)

instance IpfsOperation OpObjectData where
  type IpfsResponse OpObjectData = T.Text
  toHttpInfo (OpObjectData key) = IpfsHttpInfo GetText ["object", "data"] query
    where query = singletonQuery "arg" key

data ObjectDiff = ObjectDiff
  { diffType :: Int
  , diffPath :: T.Text
  } deriving (Show, Generic)

instance FromJSON ObjectDiff where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

data ObjectDiffs = ObjectDiffs
  { objectDiffs :: [ObjectDiff]
  } deriving (Show, Generic)

instance FromJSON ObjectDiffs where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |https://docs.ipfs.io/reference/api/http/#api-v0-object-diff
data OpObjectDiff = OpObjectDiff B.ByteString B.ByteString
  deriving (Show)

instance IpfsOperation OpObjectDiff where
  type IpfsResponse OpObjectDiff = ObjectDiffs
  toHttpInfo (OpObjectDiff key key') = IpfsHttpInfo Get ["object", "diff"] query
    where query = newQuery [ toQueryItem "arg" key
                           , toQueryItem "arg" key'
                           ]

-- |https://docs.ipfs.io/reference/api/http/#api-v0-object-get
data OpGetObject = OpGetObject B.ByteString
  deriving (Show, Generic)

instance IpfsOperation OpGetObject where
  type IpfsResponse OpGetObject = IpfsObject
  toHttpInfo (OpGetObject key) = IpfsHttpInfo Get ["object", "get"] query
    where query = singletonQuery "arg" key

-- |https://docs.ipfs.io/reference/api/http/#api-v0-object-links
data OpObjectLinks = OpObjectLinks B.ByteString
  deriving (Show, Generic)

instance IpfsOperation OpObjectLinks where
  type IpfsResponse OpObjectLinks = IpfsObjectMetadata
  toHttpInfo (OpObjectLinks key) = IpfsHttpInfo Get ["object", "links"] query
    where query = singletonQuery "arg" key

-- |https://docs.ipfs.io/reference/api/http/#api-v0-object-new
data OpCreateObject = OpCreateObject (Maybe B.ByteString)
  deriving (Show, Generic)

instance Default OpCreateObject

instance IpfsOperation OpCreateObject where
  type IpfsResponse OpCreateObject = IpfsObjectMetadata
  toHttpInfo (OpCreateObject template) = IpfsHttpInfo Get ["object", "new"] query
    where query = singletonQuery "arg" template
