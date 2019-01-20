module TestUtil
  (
    ipfsSatisfies
  ) where

import Test.Hspec

ipfsSatisfies :: (Show b) => Either String b -> (b -> Bool) -> Expectation
ipfsSatisfies (Left err) _ = expectationFailure err
ipfsSatisfies (Right resp) pred = resp `shouldSatisfy` pred
infixr 9 `ipfsSatisfies`
