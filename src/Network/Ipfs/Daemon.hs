
-- |Operations about the IPFS daemon itself.
-- These are more like meta-operations than actual IPFS operations;
-- for standard IPFS actions, use of this module is not required.
module Network.Ipfs.Daemon
  (
    OpCommands(..)
  , IpfsCommands(..)
  , IpfsCommandOptions(..)
  , OpApplyConfigProfile(..)
  , OpConfigShow(..)
  ) where

import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Default(Default)

import Network.Ipfs.Core

-- |https://docs.ipfs.io/reference/api/http/#api-v0-commands
data OpCommands = OpCommands Bool
  deriving (Show)

data IpfsCommandOptions = IpfsCommandOptions
  { optionNames :: [T.Text]
  } deriving (Show, Generic)

instance FromJSON IpfsCommandOptions where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- |The response type for the 'OpCommands' operation.
data IpfsCommands = IpfsCommands
  { commandName :: T.Text
  , commandSubcommands :: [IpfsCommands]
  , commandOptions :: IpfsCommandOptions
  } deriving (Show, Generic)

instance FromJSON IpfsCommands where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

instance IpfsOperation OpCommands where
  type IpfsResponse OpCommands = IpfsCommands
  toHttpInfo (OpCommands flags) = IpfsHttpInfo Get ["commands"] query
    where
      query = newQuery [ toQueryItem "flags" flags ]

-- |https://docs.ipfs.io/reference/api/http/#api-v0-config-profile-apply
data OpApplyConfigProfile = OpApplyConfigProfile B.ByteString
  deriving (Show)

instance IpfsOperation OpApplyConfigProfile where
  type IpfsResponse OpApplyConfigProfile = T.Text
  toHttpInfo (OpApplyConfigProfile name) = IpfsHttpInfo GetText ["config", "profile", "apply"] query
    where
      query = newQuery [ toQueryItem "arg" name ]

-- |https://docs.ipfs.io/reference/api/http/#api-v0-config-show
data OpConfigShow = OpConfigShow
  deriving (Show)

instance IpfsOperation OpConfigShow where
  type IpfsResponse OpConfigShow = T.Text
  toHttpInfo _ = IpfsHttpInfo GetText ["config", "show"] emptyQuery

-- |https://docs.ipfs.io/reference/api/http/#api-v0-diag-cmds-clear
data OpDiagClear = OpDiagClear
  deriving (Show)

instance IpfsOperation OpDiagClear where
  type IpfsResponse OpDiagClear = T.Text
  toHttpInfo _ = IpfsHttpInfo GetText ["diag", "cmds", "clear"] emptyQuery

-- |https://docs.ipfs.io/reference/api/http/#api-v0-diag-cmds-set-time
data OpDiagSetTime = OpDiagSetTime B.ByteString -- TODO: this should be a strong time type?
  deriving (Show)

instance IpfsOperation OpDiagSetTime where
  type IpfsResponse OpDiagSetTime = T.Text
  toHttpInfo (OpDiagSetTime time) = IpfsHttpInfo GetText ["diag", "cmds", "set-time"] query
    where
      query = newQuery [ toQueryItem "arg" time ]

-- |https://docs.ipfs.io/reference/api/http/#api-v0-diag-sys
data OpDiagSys = OpDiagSys
  deriving (Show)

instance IpfsOperation OpDiagSys where
  type IpfsResponse OpDiagSys = T.Text
  toHttpInfo _ = IpfsHttpInfo GetText ["diag", "sys"] emptyQuery
