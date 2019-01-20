import Control.Concurrent
import System.Process
import Test.Hspec

import BootstrapSpec(bootstrapSpec)
import KeysSpec(keysSpec)

ignoreHookContext :: SpecWith () -> SpecWith a
ignoreHookContext = aroundWith (\actionRunner -> const (actionRunner ()))

-- TODO: suppres stdout from daemon
startIpfsDaemon :: IO ProcessHandle
startIpfsDaemon = do
  handle <- spawnCommand "ipfs daemon"
  threadDelay 5000000
  return handle

main :: IO ()
main = hspec $ beforeAll startIpfsDaemon . afterAll terminateProcess $ do
  ignoreHookContext $ do
    bootstrapSpec
    keysSpec
