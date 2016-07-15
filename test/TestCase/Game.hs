module TestCase.Game (tests)
where

import Test.Framework.Providers.HUnit (testCase)
import Test.Framework (Test, testGroup)
import Test.HUnit ((@=?))

import Game

tests :: [Test]
tests =
    [ testGroup "snapIfTrue(Top/Left)"
        [ testCase "snapIfTrue x - 1"
            $ 9 @=? snapIfTrue 0 ((flip (-)) 1) (>) 10
        , testCase "snapIfTrue x - 10 snap to zero from 1"
            $ 0 @=? snapIfTrue 0 ((flip (-)) 10) (>) 1
        , testCase "snapIfTrue x - 1 snap to 0 from 0"
            $ 0 @=? snapIfTrue 0 ((flip (-)) 1) (>) 0
        , testCase "snapIfTrue x - 1 snap to 0 from negative value"
            $ 0 @=? snapIfTrue 0 ((flip (-)) 1) (>) (-10)
        ]
    , testGroup "snapIfTrue(Bottom/Right)"
        [ testCase "snapIfTrue x + 1" $ 11 @=? snapIfTrue 15 (+ 1) (<) 10
        , testCase "snapIfTrue x + 10 snap to 15"
            $ 15 @=? snapIfTrue 15 (+ 10) (<) 10
        ]
    ]

