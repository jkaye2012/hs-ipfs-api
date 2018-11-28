{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |Information about the current IPFS swarm.
module Network.Ipfs.Swarm
  (
    SwarmPeers
  , SwarmPeer
  , OpSwarmPeers
  , opSwarmPeers
  , verbose
  , withLatency
  , withStreams
    -- * Swarm addresses
  , SwarmAddrs
  , OpSwarmListenAddrs
  , opSwarmListenAddrs
  , OpSwarmLocalAddrs
  , opSwarmLocalAddrs
  , withIds
  ) where

import qualified Data.ByteString as B

import Network.Ipfs.Core

-- ** Peers

newtype OpSwarmPeers = OpSwarmPeers { swarmPeersQuery :: IpfsQuery }
  deriving (Show)

-- |Creates a new swarm peer list operation.
-- Can be used with the 'verbose', 'withLatency', and 'withStreams' functions.
-- https://docs.ipfs.io/reference/api/http/#api-v0-swarm-peers
opSwarmPeers :: OpSwarmPeers
opSwarmPeers = OpSwarmPeers emptyQuery

verbose :: OpSwarmPeers -> OpSwarmPeers
verbose = OpSwarmPeers . updateQuery ("verbose", Nothing) . swarmPeersQuery

withLatency :: OpSwarmPeers -> OpSwarmPeers
withLatency = OpSwarmPeers . updateQuery ("latency", Nothing) . swarmPeersQuery

withStreams :: OpSwarmPeers -> OpSwarmPeers
withStreams = OpSwarmPeers . updateQuery ("streams", Nothing) . swarmPeersQuery

data SwarmPeer = SwarmPeer
  { swarmpeerAddr :: String
  } deriving (Show, Generic)

instance FromJSON SwarmPeer where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

data SwarmPeers = SwarmPeers
  { swarmPeers :: [SwarmPeer]
  } deriving (Show, Generic)

instance FromJSON SwarmPeers where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpSwarmPeers where
  type IpfsResponse OpSwarmPeers = SwarmPeers
  toHttpInfo = IpfsHttpInfo Get ["swarm", "peers"] . swarmPeersQuery

data SwarmAddrs = SwarmAddrs
  { addrStrings :: [String]
  } deriving (Show, Generic)

instance FromJSON SwarmAddrs where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- ** Listening addresses

-- |https://docs.ipfs.io/reference/api/http/#api-v0-swarm-addrs-listen
data OpSwarmListenAddrs = OpSwarmListenAddrs ()
  deriving (Show)

opSwarmListenAddrs :: OpSwarmListenAddrs
opSwarmListenAddrs = OpSwarmListenAddrs ()

instance IpfsOperation OpSwarmListenAddrs where
  type IpfsResponse OpSwarmListenAddrs = SwarmAddrs
  toHttpInfo _ = IpfsHttpInfo Get ["swarm", "addrs", "listen"] emptyQuery

-- ** Local addresses

-- |https://docs.ipfs.io/reference/api/http/#api-v0-swarm-addrs-local
newtype OpSwarmLocalAddrs = OpSwarmLocalAddrs { swarmLocalAddrsQuery :: IpfsQuery }
  deriving (Show)

opSwarmLocalAddrs :: OpSwarmLocalAddrs
opSwarmLocalAddrs = OpSwarmLocalAddrs emptyQuery

withIds :: OpSwarmLocalAddrs -> OpSwarmLocalAddrs
withIds = OpSwarmLocalAddrs . updateQuery ("id", Nothing) . swarmLocalAddrsQuery

instance IpfsOperation OpSwarmLocalAddrs where
  type IpfsResponse OpSwarmLocalAddrs = SwarmAddrs
  toHttpInfo = IpfsHttpInfo Get ["swarm", "addrs", "local"] . swarmLocalAddrsQuery

-- ** Create a swarm connection

-- |https://docs.ipfs.io/reference/api/http/#api-v0-swarm-connect 
data OpSwarmConnect = OpSwarmConnect B.ByteString
  deriving (Show)

instance IpfsOperation OpSwarmConnect where
  type IpfsResponse OpSwarmConnect = SwarmAddrs
  toHttpInfo (OpSwarmConnect addr) =
    let query = newQuery [IpfsQueryItem ("arg", Just addr)] 
    in IpfsHttpInfo Get ["swarm", "connect"] query

-- ** Disconnect a previously created swarm connection

-- |https://docs.ipfs.io/reference/api/http/#api-v0-swarm-disconnect
data OpSwarmDisconnect = OpSwarmDisconnect B.ByteString
  deriving (Show)

instance IpfsOperation OpSwarmDisconnect where
  type IpfsResponse OpSwarmDisconnect = SwarmAddrs
  toHttpInfo (OpSwarmDisconnect addr) =
    let query = newQuery [IpfsQueryItem ("arg", Just addr)] 
    in IpfsHttpInfo Get ["swarm", "connect"] query

-- ** Add a swarm filter

-- |https://docs.ipfs.io/reference/api/http/#api-v0-swarm-filters-add
data OpSwarmAddFilter = OpSwarmAddFilter B.ByteString
  deriving (Show)

instance IpfsOperation OpSwarmAddFilter where
  type IpfsResponse OpSwarmAddFilter = SwarmAddrs
  toHttpInfo (OpSwarmAddFilter addr) =
    let query = newQuery [IpfsQueryItem ("arg", Just addr)] 
    in IpfsHttpInfo Get ["swarm", "connect"] query

-- ** Remove a swarm filter

-- |https://docs.ipfs.io/reference/api/http/#api-v0-swarm-filters-rm
data OpSwarmRemoveFilter = OpSwarmRemoveFilter B.ByteString
  deriving (Show)

instance IpfsOperation OpSwarmRemoveFilter where
  type IpfsResponse OpSwarmRemoveFilter = SwarmAddrs
  toHttpInfo (OpSwarmRemoveFilter addr) =
    let query = newQuery [IpfsQueryItem ("arg", Just addr)] 
    in IpfsHttpInfo Get ["swarm", "connect"] query
