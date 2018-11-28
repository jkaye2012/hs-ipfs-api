{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |Operations for interacting with block operations.
module Network.Ipfs.Block
  (
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
