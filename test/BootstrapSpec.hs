module BootstrapSpec
  (
    bootstrapSpec
  ) where

import Test.Hspec

import Network.Ipfs.Core
import Network.Ipfs.Bootstrap

bootstrapSpec :: Spec
bootstrapSpec = describe "Bootstrap" $ do
  it "peers are not empty" $ do
    body <- performIpfsOperation defaultConnectionInfo OpBootstrapList
    case body of
      Left err -> expectationFailure err
      Right peers -> peers `shouldSatisfy` (> 0) . length . bootstrapPeers
