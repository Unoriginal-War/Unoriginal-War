module TestCase (tests)
where

import Test.Framework (Test, testGroup)
import qualified TestCase.Game as Game


tests :: [Test]
tests =
    [ testGroup "TestCase.Rpc.Protocol.Type.RouteDestination"
        Game.tests
    ]
