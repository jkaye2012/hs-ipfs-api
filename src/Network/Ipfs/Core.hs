{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Ipfs.Core
  (
    -- * Query types
    IpfsQueryItem(..)
  , IpfsQuery
    -- ** Query utility functions
  , ToQueryItem(..)
  , newQuery
  , singletonQuery
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
import Control.Lens
import Data.Aeson (FromJSON(..), genericParseJSON, Value(..), fromJSON, Result(..))
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import Data.Binary.Builder (Builder, fromLazyByteString, append, toLazyByteString)
import Data.ByteString.Conversion (fromByteString')
import Data.ByteString.Lazy.Char8 (pack, unpack)
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
  , ipfsPort :: Int               -- ^ The port to which the client should connect.
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
  let port = pack . show $ ipfsPort
      version = apiVersionUriPart ipfsVersion
  in
    fromLazyByteString $ BL.concat [http, ipfsHost, ":", port, "/api/", version, "/"]

-- |A single query parameter that may be sent to the IPFS daemon.
-- We model default parameters directly to make the client binding simple and consistent;
-- every applicable operation can be given a record type of options that satisfy the
-- 'Data.Default.Default' typeclass.
data IpfsQueryItem = IpfsQueryItem QueryItem -- ^ A query parameter that has been set by the user.
                   | Default -- ^ A query parameter that should use the API default.
                   deriving (Show)

-- |A newtype wrapper modeling query parameters for an IPFS operation.
-- While these queries will generally be built atomically, the type supports incremental
-- construction of queries should such use be necessary in the future.
newtype IpfsQuery = IpfsQuery [IpfsQueryItem]
  deriving (Show)

-- |Types that can be converted to an 'IpfsQueryItem'.
-- This typeclass allows us to write operation parameters as simple record types.
-- The type can then be almost automatically collapsed into queries.
-- TODO: GHC.Generics can probably be used to fully automate this process.
class ToQueryItem a where
  toQueryItem :: B.ByteString -> a -> IpfsQueryItem
  toQueryItem' :: (B.ByteString, a) -> IpfsQueryItem
  toQueryItem' (k, v) = toQueryItem k v

instance ToQueryItem Bool where
  toQueryItem arg True = IpfsQueryItem (arg, Just "true")
  toQueryItem arg False = IpfsQueryItem (arg, Just "false")

instance ToQueryItem B.ByteString where
  toQueryItem arg val = IpfsQueryItem (arg, Just val)

instance ToQueryItem Int where
  toQueryItem arg val = IpfsQueryItem (arg, Just (BL.toStrict . pack $ show val))

instance ToQueryItem () where
  toQueryItem arg _ = IpfsQueryItem (arg, Nothing)

instance (ToQueryItem a) => ToQueryItem (Maybe a) where
  toQueryItem _ Nothing = Default
  toQueryItem arg (Just thing) = toQueryItem arg thing

-- |Constructs a new query from a list of 'IpfsQueryItem'.
-- This function is very useful in combination with 'toQueryItem' for applicable types.
newQuery :: [IpfsQueryItem] -> IpfsQuery
newQuery = IpfsQuery

-- |Constructs a new query using a single key and value.
singletonQuery :: (ToQueryItem a) => B.ByteString -> a -> IpfsQuery
singletonQuery arg val = newQuery [toQueryItem arg val]

-- |Constructs a new empty query.
emptyQuery :: IpfsQuery
emptyQuery = newQuery []

-- |Adds a single item to an existing 'IpfsQuery'.
updateQuery :: ToQueryItem a => B.ByteString -> a -> IpfsQuery -> IpfsQuery
updateQuery arg item (IpfsQuery query) = IpfsQuery $ (toQueryItem arg item) : query

-- |Coerces an 'IpfsQuery' to its corresponding 'Builder'.
-- This is only meant to be used after the entirety of a query has been constructed.
renderQuery :: IpfsQuery -> Builder
renderQuery (IpfsQuery items) = renderQueryBuilder True $ foldr toQuery [] items
  where
    toQuery x acc = case x of
      Default -> acc
      (IpfsQueryItem item) -> item : acc

data HttpMethod = Get
                | GetText
                | Post Part -- TODO: should add an indirection here so that binding users aren't exposed to Wreq
                | PostText Part
                deriving (Show)

type PathSegments = [BL.ByteString]

data IpfsHttpInfo = IpfsHttpInfo HttpMethod PathSegments IpfsQuery
  deriving (Show)

renderEndpoints :: PathSegments -> Builder
renderEndpoints = fromLazyByteString . BL.intercalate "/"

-- |Models an individual IPFS API operation.
class (FromJSON (IpfsResponse a)) => IpfsOperation a where
  type IpfsResponse a :: * -- ^ The type that the client should expect to receive back from the API.
  toHttpInfo :: a -> IpfsHttpInfo -- ^ Converts the operation to its outgoing API representation.

responseToText :: (FromJSON a) => Response BL.ByteString -> Response a
responseToText resp =
  let res = do
        body <- fromByteString' $ resp ^. responseBody
        return $ fromJSON $ String body
  in
    case res of
      Just (Success r) -> fmap (const r) resp
      Just (Error err) -> error "failed to parse response as text"
      otherwise -> error "failed to convert bytestring to text; encoding issue?"

-- |Converts a failed Wreq response to an error string for the user.
responseToError :: (Show a) => Response a -> String
responseToError = show -- TODO: should do something better here

-- |Performs an IPFS API operation.
performIpfsOperation :: (IpfsOperation a, Show (IpfsResponse a)) => IpfsConnectionInfo -> a -> IO (Either String (IpfsResponse a))
performIpfsOperation conn op =
  let (IpfsHttpInfo method path query) = toHttpInfo op
      root = apiRoot conn
      endp = renderEndpoints path
      qp = Network.Ipfs.Core.renderQuery query
      url = unpack $ toLazyByteString (root `append` endp `append` qp)
  in
    do -- TODO: handle exceptions?
      response <- case method of
                    Get -> do
                      r <- asJSON =<< get url
                      return r
                    GetText -> do
                      resp <- get url
                      return $ responseToText resp
                    (Post body) -> do
                      r <- asJSON =<< post url body
                      return r
                    (PostText body) -> do
                      resp <- post url body
                      return $ responseToText resp
      if response ^. responseStatus == ok200
        then return $ Right $ response ^. responseBody
        else return $ Left $ responseToError response
