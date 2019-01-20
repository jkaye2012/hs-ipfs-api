{-# LANGUAGE OverloadedStrings #-}

module KeysSpec
  (
    keysSpec
  ) where

import qualified Data.Text as T
import Data.Default
import Test.Hspec

import Network.Ipfs.Core
import Network.Ipfs.Keys
import TestUtil

hasKey :: T.Text -> IpfsKeyList -> Bool
hasKey name = elem name . fmap keyName . listKeys

keysSpec :: Spec
keysSpec = describe "Keys API" $ do
  it "can list keys" $ do
    resp <- performIpfsOperation defaultConnectionInfo OpKeyList
    resp `ipfsSatisfies` not . hasKey "ApiUnitTest"
  it "can generate a new key" $ do
    resp <- performIpfsOperation defaultConnectionInfo (OpKeyGen "ApiUnitTest" def)
    resp `ipfsSatisfies` (== "ApiUnitTest") . keyName
  it "can rename an existing key" $ do
    resp <- performIpfsOperation defaultConnectionInfo (OpKeyRename "ApiUnitTest" "TestRename" def)
    resp `ipfsSatisfies` (== "TestRename") . renamedNow
  it "can remove an existing key" $ do
    resp <- performIpfsOperation defaultConnectionInfo (OpKeyRemove "TestRename")
    resp `ipfsSatisfies` hasKey "TestRename"
