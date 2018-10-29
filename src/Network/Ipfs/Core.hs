{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Ipfs.Core
  (
    -- * Query types
    IpfsQueryItem
  , IpfsQuery
    -- ** Query utility functions
  , newQuery
  , updateQuery
  , emptyQuery
    -- * API operation building blocks
  , IpfsHttpInfo(..)
  , IpfsOperation(..)
  , doThing
  , doOtherThing
  , defaultConnectionInfo
    -- * Convenience re-exports
  , Generic
  , FromJSON(..)
  , genericParseJSON
  , aesonPrefix
  , pascalCase
  ) where

import qualified Data.ByteString.Lazy as B
import qualified Data.Set as S
import qualified Data.Text as T
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.Binary (Binary)
import Data.Binary.Builder (Builder, fromLazyByteString, append, toLazyByteString)
import Data.ByteString.Lazy.Char8 (unpack)
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
    fromLazyByteString $ B.concat [https, ipfsHost, ":", port, "/api/", version, "/"]

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

type PathSegments = [B.ByteString]

type IpfsQuery = S.Set IpfsQueryItem

data IpfsHttpInfo = IpfsHttpInfo PathSegments IpfsQuery
  deriving (Show)

renderEndpoints :: PathSegments -> Builder
renderEndpoints = fromLazyByteString . B.intercalate "/"

class (FromJSON (IpfsResponse a)) => IpfsOperation a where
  type IpfsResponse a :: *
  toHttpInfo :: a -> IpfsHttpInfo

newQuery :: [IpfsQueryItem] -> IpfsQuery
newQuery = S.fromList 

emptyQuery :: IpfsQuery
emptyQuery = newQuery []

updateQuery :: QueryItem -> IpfsQuery -> IpfsQuery
updateQuery q i = S.insert (IpfsQueryItem q) i

renderQuery :: IpfsQuery -> Builder
renderQuery = renderQueryBuilder True . fmap getQueryItem . S.toList

doThing :: (IpfsOperation a) => a -> IO (Response (IpfsResponse a))
doThing op = do
  r <- asJSON =<< get "http://127.0.0.1:5001/api/v0/swarm/addrs/listen" 
  return r

doOtherThing :: (IpfsOperation a) => IpfsConnectionInfo -> a -> IO (Response (IpfsResponse a))
doOtherThing conn op =
  let (IpfsHttpInfo path query) = toHttpInfo op
      root = apiRoot conn
      endp = renderEndpoints path 
      qp = Network.Ipfs.Core.renderQuery query
      url = unpack $ toLazyByteString (root `append` endp `append` qp)
  in
    do
      putStrLn url
      r <- asJSON =<< get url 
      return r


