{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Network.Ipfs.Core
  (
    -- * Query types
    IpfsQueryItem(..)
  , IpfsQuery
    -- ** Query utility functions
  , ToQueryItem(..)
  , newQuery
  , updateQuery
  , emptyQuery
    -- * API operation building blocks
  , HttpMethod(..)
  , IpfsHttpInfo(..)
  , IpfsOperation(..)
  , performIpfsOperation
  , defaultConnectionInfo
    -- * Convenience re-exports
  , Generic
  , FromJSON(..)
  , genericParseJSON
  , aesonPrefix
  , pascalCase
  , Part
  ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
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
apiVersionUriPart :: IpfsApiVersion -> BL.ByteString
apiVersionUriPart V0 = "v0"

-- |Connection information for the client. Usually, this would point to an IPFS daemon
-- running on the local host.
data IpfsConnectionInfo = IpfsConnectionInfo
  { ipfsHost :: BL.ByteString      -- ^ The host to which the client should connect.
  , ipfsPort :: Int               -- ^ The post to which the client should connect.
  , ipfsVersion :: IpfsApiVersion -- ^ The version of the IPFS HTTP API served by the daemon.
  } deriving (Show)

-- |The default IPFS connection.
defaultConnectionInfo :: IpfsConnectionInfo
defaultConnectionInfo = IpfsConnectionInfo { ipfsHost = "127.0.0.1"
                                           , ipfsPort = 5001
                                           , ipfsVersion = V0
                                           }

-- |The http URL prefix.
http :: BL.ByteString
http = "http://"

-- |Constructs the URI root for a given IPFS HTTP connection.
-- This root is common across all API operations; as such, the root builder is used as
-- the starting point for all client functionality.
apiRoot :: IpfsConnectionInfo -> Builder
apiRoot (IpfsConnectionInfo{..}) =
  let port = toByteString ipfsPort
      version = apiVersionUriPart ipfsVersion
  in
    fromLazyByteString $ BL.concat [http, ipfsHost, ":", port, "/api/", version, "/"]

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

data HttpMethod = Get
                | Post Part
                deriving (Show)

type PathSegments = [BL.ByteString]

type IpfsQuery = S.Set IpfsQueryItem

data IpfsHttpInfo = IpfsHttpInfo HttpMethod PathSegments IpfsQuery
  deriving (Show)

renderEndpoints :: PathSegments -> Builder
renderEndpoints = fromLazyByteString . BL.intercalate "/"

-- |Models an individual IPFS API operation.
class (FromJSON (IpfsResponse a)) => IpfsOperation a where
  type IpfsResponse a :: * -- ^ The type that the client should expect to receive back from the API.
  toHttpInfo :: a -> IpfsHttpInfo -- ^ Converts the operation to its outgoing API representation.
  
-- |Types that can be converted to a QueryItem
-- This is useful for more complex operations that need to have their parameters
-- converted mostly by hand.
class ToQueryItem a where
  toQueryItem :: B.ByteString -> a -> IpfsQueryItem

instance ToQueryItem Bool where
  toQueryItem arg True = IpfsQueryItem (arg, Just "true")
  toQueryItem arg False = IpfsQueryItem (arg, Just "false")

instance ToQueryItem B.ByteString where
  toQueryItem arg val = IpfsQueryItem (arg, Just val)

-- |Constructs a new query from a list of 'IpfsQueryItem'.
-- If multiple conflicting items are passed, the first one will be taken.
newQuery :: [IpfsQueryItem] -> IpfsQuery
newQuery = S.fromList 

-- |Constructs a new empty query. Useful if all potential components of a query are optional.
emptyQuery :: IpfsQuery
emptyQuery = newQuery []

-- |Adds a single item to an existing 'IpfsQuery'
updateQuery :: QueryItem -> IpfsQuery -> IpfsQuery
updateQuery q i = S.insert (IpfsQueryItem q) i

-- |Coerces an 'IpfsQuery' to its corresponding 'Builder'.
-- This is only meant to be used after the entirety of a query has been constructed.
renderQuery :: IpfsQuery -> Builder
renderQuery = renderQueryBuilder True . fmap getQueryItem . S.toList

-- |Performs a single IPFS API operation.
performIpfsOperation :: (IpfsOperation a) => IpfsConnectionInfo -> a -> IO (Response (IpfsResponse a))
performIpfsOperation conn op =
  let (IpfsHttpInfo method path query) = toHttpInfo op
      root = apiRoot conn
      endp = renderEndpoints path 
      qp = Network.Ipfs.Core.renderQuery query
      url = unpack $ toLazyByteString (root `append` endp `append` qp)
  in
    case method of
      Get -> do r <- asJSON =<< get url 
                return r
      (Post body) -> do r <- asJSON =<< post url body
                        return r


