{-# LANGUAGE RecordWildCards #-}

-- |Operations for adding files to IPFS.
module Network.Ipfs.File
  (
    FileResponse(..)
  , AddFileOptions(..)
  , OpAddFile(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Default(Default)
import Data.Word

import Network.Ipfs.Core

data AddFileOptions = AddFileOptions
  { addFileRecursive :: Bool
  , addFileQuiet :: Bool
  , addFileQuieter :: Bool
  , addFileSilent :: Bool
  , addFileProgress :: Bool
  , addFileTrickle :: Bool
  , addFileOnlyHash :: Bool
  , addFileWrapDirectory :: Bool
  , addFileHidden :: Bool
  , addFileChunker :: B.ByteString
  , addFilePin :: Bool
  , addFileRawLeaves :: Bool
  , addFileNoCopy :: Bool
  , addFileCache :: Bool
  , addFileHash :: B.ByteString
  } deriving (Show)

defaultAddFileOptions :: AddFileOptions
defaultAddFileOptions = AddFileOptions
  { addFileRecursive = False
  , addFileQuiet = False
  , addFileQuieter = False
  , addFileSilent = False
  , addFileProgress = False
  , addFileTrickle = False
  , addFileOnlyHash = False
  , addFileWrapDirectory = False
  , addFileHidden = False
  , addFileChunker = "size-262144"
  , addFilePin = True
  , addFileRawLeaves = False
  , addFileNoCopy = False
  , addFileCache = False
  , addFileHash = "sha2-256"
  }

-- |The response type for the 'OpAddFile' operation.
data FileResponse = FileResponse
  { fileName :: T.Text
  , fileHash :: T.Text
  , fileSize :: T.Text
  } deriving (Show, Generic)

instance FromJSON FileResponse where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |https://docs.ipfs.io/reference/api/http/#api-v0-add
data OpAddFile = OpAddFile AddFileOptions Part
  deriving (Show)

instance IpfsOperation OpAddFile where
  type IpfsResponse OpAddFile = FileResponse
  toHttpInfo (OpAddFile AddFileOptions{..} file) = IpfsHttpInfo (Post file) ["add"] query
    where query = newQuery [ toQueryItem "recursive" addFileRecursive
                           , toQueryItem "quiet" addFileQuiet
                           , toQueryItem "quieter" addFileQuieter
                           , toQueryItem "silent" addFileSilent
                           , toQueryItem "progess" addFileProgress
                           , toQueryItem "trickle" addFileTrickle
                           , toQueryItem "only-hash" addFileOnlyHash
                           , toQueryItem "wrap-with-directory" addFileWrapDirectory
                           , toQueryItem "hidden" addFileHidden
                           , toQueryItem "pin" addFilePin
                           , toQueryItem "raw-leaves" addFileRawLeaves
                           , toQueryItem "nocopy" addFileNoCopy
                           , toQueryItem "fscache" addFileCache
                           , toQueryItem "hash" addFileHash
                           ]

data CatFileOptions = CatFileOptions
  { catOffset :: Int
  , catLength :: Maybe Int
  } deriving (Show, Generic)

instance Default CatFileOptions

-- |https://docs.ipfs.io/reference/api/http/#api-v0-cat
data OpCatFile = OpCatFile B.ByteString CatFileOptions
  deriving (Show)

instance IpfsOperation OpCatFile where
  type IpfsResponse OpCatFile = T.Text
  toHttpInfo (OpCatFile path CatFileOptions{..}) = IpfsHttpInfo GetText ["cat"] query
    where
      query = newQuery [ toQueryItem "arg" path
                       , toQueryItem "offset" catOffset
                       , toQueryItem "length" catLength
                       ]

-- |Options for the 'OpResolveDns' operation.
data DnsResolutionOptions = DnsResolutionOptions
  { resolveRecursive :: Maybe Bool
  } deriving (Show, Generic)

instance Default DnsResolutionOptions

-- |https://docs.ipfs.io/reference/api/http/#api-v0-dns
data OpResolveDns = OpResolveDns B.ByteString DnsResolutionOptions
  deriving (Show)

-- |The response type for the 'OpResolveDns' operation.
data DnsResolution = DnsResolution
  { resolutionPath :: T.Text
  } deriving (Show, Generic)

instance FromJSON DnsResolution where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpResolveDns where
  type IpfsResponse OpResolveDns = DnsResolution
  toHttpInfo (OpResolveDns path DnsResolutionOptions{..}) = IpfsHttpInfo Get ["dns"] query
    where
      query = newQuery [ toQueryItem "arg" path
                       , toQueryItem "recursive" resolveRecursive
                       ]

-- |https://docs.ipfs.io/reference/api/http/#api-v0-file-ls
data OpListDirectory = OpListDirectory B.ByteString
  deriving (Show)

data DirectoryLink = DirectoryLink
  { dirlinkName :: T.Text
  , dirlinkHash :: T.Text
  , dirlinkSize :: Word64
  , dirlinkType :: T.Text
  } deriving (Show, Generic)

data DirectoryObject = DirectoryObject
  { dirobjectHash :: T.Text
  , dirobjectSize :: Word64
  , dirobjectType :: T.Text
  , dirobjectLinks :: [DirectoryLink]
  } deriving (Show, Generic)

-- |The response type for the 'OpListDirectory' operation.
data DirectoryContents = DirectoryContents
  { directoryArguments :: M.Map T.Text T.Text
  , directoryObjects :: M.Map T.Text DirectoryObject
  } deriving (Show, Generic)

instance FromJSON DirectoryLink where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance FromJSON DirectoryObject where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance FromJSON DirectoryContents where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpListDirectory where
  type IpfsResponse OpListDirectory = DirectoryContents
  toHttpInfo (OpListDirectory path ) = IpfsHttpInfo Get ["file", "ls"] query
    where
      query = singletonQuery "arg" path
