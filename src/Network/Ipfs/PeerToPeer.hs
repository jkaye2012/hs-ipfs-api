{-# LANGUAGE RecordWildCards #-}

-- |Operations for interacting with the IPFS p2p api.
module Network.Ipfs.PeerToPeer
  (
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Default(Default)

import Network.Ipfs.Core

-- * Shared response types

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

-- * Operation types

-- ** https://docs.ipfs.io/reference/api/http/#api-v0-p2p-listener-close

data CloseListenerOptions = CloseListenerOptions
  { closeListenerProtocol :: Maybe B.ByteString
  , closeListenerAll :: Maybe Bool
  } deriving (Show, Generic)

instance Default CloseListenerOptions

data OpCloseListener = OpCloseListener CloseListenerOptions
  deriving (Show)

-- ** https://docs.ipfs.io/reference/api/http/#api-v0-p2p-listener-ls

data OpListListeners = OpListListeners
  deriving (Show)

-- ** https://docs.ipfs.io/reference/api/http/#api-v0-p2p-listener-open

data OpOpenListener = OpOpenListener B.ByteString B.ByteString
  deriving (Show)

-- ** Internal instance implementations
