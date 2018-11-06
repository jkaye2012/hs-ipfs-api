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
