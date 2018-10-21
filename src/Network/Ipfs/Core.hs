{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}


module Network.Ipfs.Core
  (
  ) where

import qualified Data.ByteString.Lazy as B
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
