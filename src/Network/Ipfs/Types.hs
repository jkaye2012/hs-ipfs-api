
module Network.Ipfs.Types
  (
    IpfsKeyType(..)
  ) where

import Data.Aeson
import Data.Default

import Network.Ipfs.Core

-- |The public-key cryptosystems available for use within IPFS.
data IpfsKeyType = Rsa
                 | Ed25519
                 deriving (Show)

instance ToJSON IpfsKeyType where
  toJSON Rsa = String "rsa"
  toJSON Ed25519 = String "ed25519"

instance ToQueryItem IpfsKeyType where
  toQueryItem arg Rsa = IpfsQueryItem (arg, Just "rsa")
  toQueryItem arg Ed25519 = IpfsQueryItem (arg, Just "ed25519")

instance Default IpfsKeyType where
  def = Rsa
