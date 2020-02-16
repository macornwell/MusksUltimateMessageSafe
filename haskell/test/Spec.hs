module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import qualified Lib

main :: IO ()
main = hspec $ do
  describe "Lib.convertNumberToLetters" $ do
    it "returns '' for 1" $ Lib._convertNumberToLetters 1 `shouldBe` ""
    it "returns '' for 2" $ Lib._convertNumberToLetters 2 `shouldBe` ""
    it "returns 'd' for 3" $ Lib._convertNumberToLetters 3 `shouldBe` "d"
    it "returns 'e' for 4" $ Lib._convertNumberToLetters 4 `shouldBe` "e"
    it "returns 'j' for 9" $ Lib._convertNumberToLetters 9 `shouldBe` "j"
    it "returns 'k' for 10" $ Lib._convertNumberToLetters 10 `shouldBe` "k"
    it "returns 'l' for 11" $ Lib._convertNumberToLetters 11 `shouldBe` "l"
    it "returns 'fmdihge' for 51238764" $ Lib._convertNumberToLetters 51238764 `shouldBe` "fmdihge"

  describe "Lib.unScrunchBinary" $ do
    it "returns '[1]' for 2" $ Lib._unScrunchBinary 2 `shouldBe` [1]
    it "returns '[3]' for 3" $ Lib._unScrunchBinary 3 `shouldBe` [3]
    it "returns '[1,1]' for 4" $ Lib._unScrunchBinary 4 `shouldBe` [1,1]
    it "returns '[1,3]' for 5" $ Lib._unScrunchBinary 5 `shouldBe` [1,3]
    it "returns '[1,3,1]' for 10" $ Lib._unScrunchBinary 10 `shouldBe` [1,3,1]
    it "returns '[3, 3, 3, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 3, 3]' for 123123" $ Lib._unScrunchBinary 123123 `shouldBe` [3, 3, 3, 1, 1, 1, 1, 1, 3, 3, 3, 3, 1, 1, 3, 3]
    it "returns '[1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1]' for 2261697700626496" $ Lib._unScrunchBinary 2261697700626496 `shouldBe` [1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 3, 1, 1, 1, 1, 1, 1]


  describe "Lib.consultBlackBallWithLetter" $ do
    it "returns '0' for 'a'" $ Lib._consultBlackBallWithLetter 'a' `shouldBe` 0
    it "returns '8' for 'b'" $ Lib._consultBlackBallWithLetter 'b' `shouldBe` 8
    it "returns '10' for 'c'" $ Lib._consultBlackBallWithLetter 'c' `shouldBe` 10
    it "returns '38' for 'u'" $ Lib._consultBlackBallWithLetter 'u' `shouldBe` 38
    it "returns '40' for 'v'" $ Lib._consultBlackBallWithLetter 'v' `shouldBe` 40
    it "returns '0' for 'w'" $ Lib._consultBlackBallWithLetter 'w' `shouldBe` 0
    it "returns '0' for 'y'" $ Lib._consultBlackBallWithLetter 'y' `shouldBe` 0
    it "returns '0' for 'z'" $ Lib._consultBlackBallWithLetter 'z' `shouldBe` 0

  describe "Lib.consultBlackBallWithNumber" $ do
    it "returns 'a' for '0'" $ Lib._consultBlackBallWithNumber 0 `shouldBe` 'a'
    it "returns 'b' for '1'" $ Lib._consultBlackBallWithNumber 1 `shouldBe` 'b'
    it "returns 'c' for '2'" $ Lib._consultBlackBallWithNumber 2 `shouldBe` 'c'
    it "returns 'i' for '8'" $ Lib._consultBlackBallWithNumber 8 `shouldBe` 'i'
    it "returns 'k' for '10'" $ Lib._consultBlackBallWithNumber 10 `shouldBe` 'k'
    it "returns '0' for '40'" $ Lib._consultBlackBallWithNumber 40 `shouldBe` 'a'
    it "returns 'u' for '20'" $ Lib._consultBlackBallWithNumber 20 `shouldBe` 'u'
    it "returns 'v' for '21'" $ Lib._consultBlackBallWithNumber 21 `shouldBe` 'v'
    it "returns 'b' for '22'" $ Lib._consultBlackBallWithNumber 22 `shouldBe` 'b'
    it "returns 'c' for '23'" $ Lib._consultBlackBallWithNumber 23 `shouldBe` 'c'

  describe "Lib.unpackToOffsetList" $ do
    it "returns '[20, 16, 0, 24, 24, 16, 32, 16, 0, 22]' for 'gfaiifqfah'" $ Lib._unpackToOffsetList "gfaiifqfah" `shouldBe` [20, 16, 0, 24, 24, 16, 32, 16, 0, 22]
    it "returns '[0]' for 'a'" $ Lib._unpackToOffsetList "a" `shouldBe` [0]
    it "returns '[8]' for 'b'" $ Lib._unpackToOffsetList "b" `shouldBe` [8]
    it "returns '[10]' for 'c'" $ Lib._unpackToOffsetList "c" `shouldBe` [10]
    it "returns '[10]' for 'c'" $ Lib._unpackToOffsetList "c" `shouldBe` [10]



