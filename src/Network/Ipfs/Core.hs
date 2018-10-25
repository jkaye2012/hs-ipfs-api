{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Ipfs.Core
  (
  ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.Binary (Binary)
import Data.Binary.Builder (Builder, fromLazyByteString, append)
import Data.ByteString.Conversion (toByteString)
import GHC.Generics (Generic)
import Network.HTTP.Types
import Network.Wreq

-- |The version of the IPFS HTTP API that the client should expect to connect to.
data IpfsApiVersion = V0
                    deriving (Show, Eq)

-- |Converts an 'IpfsApiVersion' to its corresponding URI segment. Used when constructing API endpoints.
apiVersionUriPart :: IpfsApiVersion -> B.ByteString
apiVersionUriPart V0 = "v0"

-- |Connection information for the client. Usually, this would point to an IPFS daemon
-- running on the local host.
data IpfsConnectionInfo = IpfsConnectionInfo
  {
    ipfsHost :: B.ByteString      -- ^ The host to which the client should connect.
  , ipfsPort :: Int               -- ^ The post to which the client should connect.
  , ipfsVersion :: IpfsApiVersion -- ^ The version of the IPFS HTTP API served by the daemon.
  } deriving (Show)

-- |The default IPFS connection.
defaultConnectionInfo :: IpfsConnectionInfo
defaultConnectionInfo =
  IpfsConnectionInfo { ipfsHost = "127.0.0.1"
                     , ipfsPort = 5001
                     , ipfsVersion = V0
                     }

-- |The https URL prefix.
https :: B.ByteString
https = "https://"

-- |Constructs the URI root for a given IPFS HTTP connection.
-- This root is common across all API operations; as such, the root builder is used as
-- the starting point for all client functionality.
apiRoot :: IpfsConnectionInfo -> Builder
apiRoot (IpfsConnectionInfo{..}) =
  let port = toByteString ipfsPort
      version = apiVersionUriPart ipfsVersion
  in
    fromLazyByteString $ B.concat [https, ipfsHost, ":", port, "/api/", version]

-- data IpfsOperation = OpSwarmPeers
--                    | OpBootstrapList
--                    deriving (Show)

-- pathSegments :: IpfsOperation -> [T.Text]
-- pathSegments OpSwarmPeers = ["swarm", "peers"]
-- pathSegments OpBootstrapList = ["bootstrap", "list"]

newtype IpfsQueryItem = IpfsQueryItem { getQueryItem :: QueryItem }
  deriving (Show)

instance Eq IpfsQueryItem where
  a == b = let (ak, _) = getQueryItem a
               (bk, _) = getQueryItem b
           in
             ak == bk

instance Ord IpfsQueryItem where
  a <= b = let (ak, _) = getQueryItem a
               (bk, _) = getQueryItem b
           in
             ak <= bk

type PathSegments = [T.Text]
type IpfsQuery = S.Set IpfsQueryItem

data IpfsHttpInfo = IpfsHttpInfo PathSegments IpfsQuery
  deriving (Show)

class (FromJSON (IpfsResponse a)) => IpfsOperation a where
  type IpfsResponse a :: *
  toHttpInfo :: a -> IpfsHttpInfo

newtype OpSwarmPeers = OpSwarmPeers { swarmPeersQuery :: IpfsQuery }
  deriving (Show)

updateQuery :: QueryItem -> IpfsQuery -> IpfsQuery
updateQuery q i = S.insert (IpfsQueryItem q) i

opSwarmPeers :: OpSwarmPeers
opSwarmPeers = OpSwarmPeers S.empty

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

doThing :: (IpfsOperation a) => a -> IO (Response (IpfsResponse a))
doThing op = do
  r <- asJSON =<< get "http://127.0.0.1:5001/api/v0/swarm/peers" 
  return r

  
