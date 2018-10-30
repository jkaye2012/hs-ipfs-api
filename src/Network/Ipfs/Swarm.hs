{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

-- |Information about the current IPFS swarm.
module Network.Ipfs.Swarm
  (
    -- * Swarm peer list
    SwarmPeers
  , SwarmPeer
  , OpSwarmPeers
  , opSwarmPeers
  , verbose
  , withLatency
  , withStreams
    -- * Swarm addresses
  , SwarmAddrs
    -- ** Listening addresses
  , OpSwarmListenAddrs
  , opSwarmListenAddrs
    -- ** Local addresses
  , OpSwarmLocalAddrs
  , opSwarmLocalAddrs
  , withIds
  ) where

import Network.Ipfs.Core

newtype OpSwarmPeers = OpSwarmPeers { swarmPeersQuery :: IpfsQuery }
  deriving (Show)

-- |Creates a new swarm peer list operation.
-- Meant to be used with the 'verbose', 'withLatency', and 'withStreams' functions.
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
  {
    swarmpeerAddr :: String
  } deriving (Show, Generic)

instance FromJSON SwarmPeer where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

data SwarmPeers = SwarmPeers
  {
     swarmPeers :: [SwarmPeer]
  } deriving (Show, Generic)

instance FromJSON SwarmPeers where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpSwarmPeers where
  type IpfsResponse OpSwarmPeers = SwarmPeers
  toHttpInfo = IpfsHttpInfo ["swarm", "peers"] . swarmPeersQuery

data OpSwarmListenAddrs = OpSwarmListenAddrs ()
  deriving (Show)

opSwarmListenAddrs :: OpSwarmListenAddrs
opSwarmListenAddrs = OpSwarmListenAddrs ()

data SwarmAddrs = SwarmAddrs
  {
    addrStrings :: [String]
  } deriving (Show, Generic)

instance FromJSON SwarmAddrs where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpSwarmListenAddrs where
  type IpfsResponse OpSwarmListenAddrs = SwarmAddrs
  toHttpInfo _ = IpfsHttpInfo ["swarm", "addrs", "listen"] emptyQuery

newtype OpSwarmLocalAddrs = OpSwarmLocalAddrs { swarmLocalAddrsQuery :: IpfsQuery }
  deriving (Show)

opSwarmLocalAddrs :: OpSwarmLocalAddrs
opSwarmLocalAddrs = OpSwarmLocalAddrs emptyQuery

withIds :: OpSwarmLocalAddrs -> OpSwarmLocalAddrs
withIds = OpSwarmLocalAddrs . updateQuery ("id", Nothing) . swarmLocalAddrsQuery

instance IpfsOperation OpSwarmLocalAddrs where
  type IpfsResponse OpSwarmLocalAddrs = SwarmAddrs
  toHttpInfo = IpfsHttpInfo ["swarm", "addrs", "local"] . swarmLocalAddrsQuery
