{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    , getPeers
    ) where

import Control.Monad.Catch
import Data.Aeson (FromJSON(..), genericParseJSON)
import Data.Aeson.Casing (aesonPrefix, pascalCase)
import GHC.Generics (Generic)
import Network.Wreq

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

getPeers :: IO (Response SwarmPeers)
getPeers = do
  r <- asJSON =<< get "http://127.0.0.1:5001/api/v0/swarm/peers" 
  return r

someFunc :: IO ()
someFunc = putStrLn "someFunc"
