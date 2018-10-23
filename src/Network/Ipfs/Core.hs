{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}


module Network.Ipfs.Core
  (
  ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Binary (Binary)
import Data.Binary.Builder (Builder, fromLazyByteString, append)
import Data.ByteString.Conversion (toByteString)
import Network.HTTP.Types

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

class IpfsOperation a where
  toHttpInfo :: a -> IpfsHttpInfo

-- Need to encode return value in this as well so that reponses can be properly marshalled.
-- I'm thinking that there must be some way to not have to write the boilerplate that's been
-- included in the implementation below... It's clearly very mechanical, but I haven't quite
-- been able to figure out where the right abstraction is to make things work cleanly.
data OpSwarmPeers = OpSwarmPeers IpfsQuery
  deriving (Show)

updateQuery :: IpfsQuery -> QueryItem -> IpfsQuery
updateQuery i q = S.insert (IpfsQueryItem q) i

verbose :: OpSwarmPeers -> OpSwarmPeers
verbose (OpSwarmPeers q) = OpSwarmPeers $ updateQuery q ("verbose", Nothing) 

withLatency :: OpSwarmPeers -> OpSwarmPeers
withLatency (OpSwarmPeers q) = OpSwarmPeers $ updateQuery q ("latency", Nothing)

withStreams :: OpSwarmPeers -> OpSwarmPeers
withStreams (OpSwarmPeers q) = OpSwarmPeers $ updateQuery q ("streams", Nothing)

instance IpfsOperation OpSwarmPeers where
  toHttpInfo (OpSwarmPeers q) = IpfsHttpInfo ["swarm", "peers"] q

  
