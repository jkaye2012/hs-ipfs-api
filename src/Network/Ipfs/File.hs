{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |Operations for viewing and manipulating IPFS files.
module Network.Ipfs.File
  (
  ) where

import qualified Data.ByteString as B
import qualified Data.Set as Set
import qualified Data.Text as T

import Network.Ipfs.Core

data AddFileOptions = AddFileOptions
  {
    addFileRecursive :: Bool
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
  {
    addFileRecursive = False
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

data OpAddFile = OpAddFile AddFileOptions B.ByteString
  deriving (Show)

data FileResponse = FileResponse
  { fileName :: T.Text
  , fileHash :: T.Text
  , fileBytes :: Int
  , fileSize :: T.Text
  } deriving (Show, Generic)

instance FromJSON FileResponse where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

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
  
