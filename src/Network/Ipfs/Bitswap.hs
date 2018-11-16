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

import qualified Data.ByteString as B
import qualified Data.Text as T

import Network.Ipfs.Core

-- * Bitswap ledger

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
