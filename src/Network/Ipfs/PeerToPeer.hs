{-# LANGUAGE RecordWildCards #-}

-- |Operations for interacting with the IPFS p2p api.
module Network.Ipfs.PeerToPeer
  (
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Default(Default)

import Network.Ipfs.Core

data PeerListener = PeerListener
  { listenerProtocol :: T.Text
  , listenerAddress :: T.Text
  } deriving (Show, Generic)

instance FromJSON PeerListener where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

data PeerListeners = PeerListeners
  { peerListeners :: [PeerListener]
  } deriving (Show, Generic)

instance FromJSON PeerListeners where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

data PeerStream = PeerStream
  { streamHandlerID :: T.Text
  , streamProtocol :: T.Text
  , streamLocalPeer :: T.Text
  , streamLocalAddress :: T.Text
  , streamRemotePeer :: T.Text
  , streamRemoteAddress :: T.Text
  } deriving (Show, Generic)

instance FromJSON PeerStream where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

data PeerStreams = PeerStreams
  { peerStreams :: [PeerStream]
  } deriving (Show, Generic)

instance FromJSON PeerStreams where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase
