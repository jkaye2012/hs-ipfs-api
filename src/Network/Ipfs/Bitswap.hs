{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |Operations for interacting with the bitswap data trading module.
module Network.Ipfs.Bitswap
  (
    BitswapLedger(..)
  , OpBitswapLedger(..)
  ) where

import Data.Aeson.Types (Value)
import qualified Data.ByteString as B
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Word

import Network.Ipfs.Core

-- * Bitswap ledger

-- |The return value for the 'OpBitswapLedger' operation.
data BitswapLedger = BitswapLedger
  { bitswapPeer :: T.Text
  , bitswapValue :: Double
  , bitswapSent :: Word
  , bitswapRecv :: Word
  , bitswapExchanged :: Word
  } deriving (Show, Generic)

instance FromJSON BitswapLedger where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |https://docs.ipfs.io/reference/api/http/#api-v0-bitswap-ledger
data OpBitswapLedger = OpBitswapLedger B.ByteString
  deriving Show

instance IpfsOperation OpBitswapLedger where
  type IpfsResponse OpBitswapLedger = BitswapLedger
  toHttpInfo (OpBitswapLedger peerId) =
    let query = newQuery [IpfsQueryItem ("arg", Just peerId)]
    in IpfsHttpInfo Get ["bitswap", "ledger"] query

-- * Bitswap provider

-- |https://docs.ipfs.io/reference/api/http/#api-v0-bitswap-reprovide
data OpBitswapReprovide = OpBitswapReprovide
  deriving Show

instance IpfsOperation OpBitswapReprovide where
  type IpfsResponse OpBitswapReprovide = ()
  toHttpInfo _ = IpfsHttpInfo Get ["bitswap", "reprovide"] emptyQuery

-- * Bitswap statistics

-- |The response type for the 'OpBitswapStat' operation
data BitswapStatistics = BitswapStatistics
  { bitswapBufLen :: Int
  , bitswapWantlist :: [Map.Map String String]
  , bitswapPeers :: [String]
  , bitswapBlocksReceived :: Word64
  , bitswapDataReceived :: Word64
  , bitswapBlocksSent :: Word64
  , bitswapDataSent :: Word64
  , bitswapDupBlksReceived :: Word64
  , bitswapDupDataReceived :: Word64
  } deriving (Show, Generic)

instance FromJSON BitswapStatistics where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |https://docs.ipfs.io/reference/api/http/#api-v0-bitswap-stat
data OpBitswapStat = OpBitswapStat
  deriving Show

instance IpfsOperation OpBitswapStat where
  type IpfsResponse OpBitswapStat = BitswapStatistics
  toHttpInfo _ = IpfsHttpInfo Get ["bitswap", "stat"] emptyQuery
