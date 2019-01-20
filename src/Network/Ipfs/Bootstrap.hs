
-- |Operations for interacting with the IPFS bootstrap list.
-- The IPFS bootstrap list is a list of peers with which the IPFS daemon learns about other peers on the network.
-- IPFS comes with a default list of trusted peers, but you are free to modify the list to suit your needs.
-- One popular use for a custom bootstrap list is to create a personal IPFS network.
module Network.Ipfs.Bootstrap
  (
    BootstrapPeers(..)
  , OpAddDefaultPeers(..)
  , OpBootstrapList(..)
  , OpRemovePeers(..)
  ) where

import qualified Data.Text as T

import Network.Ipfs.Core

-- |The response type for all bootstrap operations.
data BootstrapPeers = BootstrapPeers
  { bootstrapPeers :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON BootstrapPeers where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |https://docs.ipfs.io/reference/api/http/#api-v0-bootstrap-add-default
data OpAddDefaultPeers = OpAddDefaultPeers
  deriving (Show)

instance IpfsOperation OpAddDefaultPeers where
  type IpfsResponse OpAddDefaultPeers = BootstrapPeers
  toHttpInfo _ = IpfsHttpInfo Get ["bootstrap", "add", "default"] emptyQuery

-- |https://docs.ipfs.io/reference/api/http/#api-v0-bootstrap-list
data OpBootstrapList = OpBootstrapList
  deriving (Show)

instance IpfsOperation OpBootstrapList where
  type IpfsResponse OpBootstrapList = BootstrapPeers
  toHttpInfo _ = IpfsHttpInfo Get ["bootstrap", "list"] emptyQuery

-- |https://docs.ipfs.io/reference/api/http/#api-v0-bootstrap-rm-all
data OpRemovePeers = OpRemovePeers
  deriving (Show)

instance IpfsOperation OpRemovePeers where
  type IpfsResponse OpRemovePeers = BootstrapPeers
  toHttpInfo _ = IpfsHttpInfo Get ["bootstrap", "rm", "all"] emptyQuery
