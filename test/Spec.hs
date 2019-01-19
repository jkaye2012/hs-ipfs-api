import Control.Concurrent
import System.Process
import Test.Hspec

import Network.Ipfs.Core
import Network.Ipfs.Bootstrap

ignoreHookContext :: SpecWith () -> SpecWith a
ignoreHookContext = aroundWith (\actionRunner -> const (actionRunner ()))

startIpfsDaemon :: IO ProcessHandle
startIpfsDaemon = do
  handle <- spawnCommand "ipfs daemon"
  threadDelay 5000000
  return handle

main :: IO ()
main = hspec $ beforeAll startIpfsDaemon . afterAll terminateProcess $ do
  ignoreHookContext $ do
    describe "thing" $ do
      it "is a test" $ do
        body <- performIpfsOperation defaultConnectionInfo OpBootstrapList
        case body of
          Left err -> expectationFailure err
          Right peers -> peers `shouldSatisfy` (> 0) . length . bootstrapPeers
