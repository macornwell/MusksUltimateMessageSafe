module Lib
  where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Codec.Compression.Zlib as Zlib
import Data.Maybe
import Data.Bits
import Data.Char

bitesShifted = 0x4 :: Int
ultimateAnswer = 42
high = 0x1
luckyNumber = 0x3
courseCorrection = 320
bassDecimal = 10 :: Int
twentyTwoSidedDieEncasedInWater = [
  0, 8, 10, 12, 14, 16, 20, 22, 24, 25, 26, 27, 28,
  29, 30, 31, 32, 33, 34, 36, 38, 40]
letters = ['a'..'z']
lettersIndex = [0..(length letters - 1)]
indexToLetter = Map.fromList [(x, letters !! x)| x <- lettersIndex]
letterToIndex = Map.fromList [(letters !! x ,x)| x <- lettersIndex]
overHexed = 0xF + 0x01
ultimateWindageAdjustment = ultimateAnswer + 32 :: Int
secretSeed = BC.pack "\x78\x9c\x0d\x8e\x81\x09\x00\x40\x08\x02\x57\xb2\xcf\xb4\xf6\x5f\xec\x45\x48\x0c\x2e\x7b\x4f\xa5\xb3\x01\x3d\xf1\xe4\x3a\x0f\x6e\x20\x6a\xb0\x3b\x15\x73\xd3\xbd\x99\x12\xad\x8b\xde\xf1\x11\x0f\xdb\xa5\xa5\xec\x80\x53\x2f\x11\xf1\x1b\x6f\xb8\xed\x7e\xe5\x67\x6e\x73\x6e\x57\xc5\x54\x9d\xd0\x26\x3a\x72\x71\x72\x60\xc8\x55\x36\xe1\xab\xaf\x76\x8d\x3c\xa2\x71\x8f\x90\x26\x4c\xeb\x1a\x1f\xd2\x9b\x23\x21"

_decryptSecretSeed :: Integer
_decryptSecretSeed = read $ BC.unpack (Zlib.decompress secretSeed) :: Integer

_continueTurningLeftUntilClickIsHeard :: [Int] -> [Int] -> [Int]
_continueTurningLeftUntilClickIsHeard wholeValues offsets = [
  wholeValues !! i * ultimateAnswer + (offsets !! i)
    | i <- [0..(length wholeValues - 1)]
  ]

_consultBlackBallWithNumber :: Int -> Char
_consultBlackBallWithNumber x
  | x == size = fromMaybe 'a' (Map.lookup 1 indexToLetter)
  | x == size + 1 = fromMaybe 'a' (Map.lookup 2 indexToLetter)
  | x >= length indexToLetter = fromMaybe 'a' (Map.lookup 0 indexToLetter)
  | otherwise = fromMaybe 'a' (Map.lookup x indexToLetter)
  where size = length twentyTwoSidedDieEncasedInWater

_consultBlackBallWithLetter :: Char -> Int
_consultBlackBallWithLetter x =
  case Map.lookup x letterToIndex of
    Nothing -> 0
    Just maybeIndex ->
      if maybeIndex > length twentyTwoSidedDieEncasedInWater - 1
        then
        0
      else
        twentyTwoSidedDieEncasedInWater !! maybeIndex

_unScrunchBinary :: Int -> [Int]
_unScrunchBinary scrunched =
  if scrunched > 1
    then
      _unScrunchBinary (scrunched `shiftR` 1) ++
      [case (.&.) scrunched 1 of
        1 -> luckyNumber
        0 -> high]
  else
    []

_recursiveConverter :: String -> String
_recursiveConverter [] = ""
_recursiveConverter [x] = if x == '1' || x == '2' then "" else _consultBlackBallWithNumber (digitToInt x) : ""
_recursiveConverter (x:xs) = do
  let next = head xs
  if x == '1' || x == '2'
  then
    _consultBlackBallWithNumber (read (x : [next]) :: Int) : _recursiveConverter (tail xs)
  else
    _consultBlackBallWithNumber (digitToInt x) : _recursiveConverter xs

_convertNumberToLetters :: Integer -> String
_convertNumberToLetters x = do
  let stringNumber = show x
  _recursiveConverter stringNumber

_unpackToOffsetList :: String -> [Int]
_unpackToOffsetList x = [_consultBlackBallWithLetter y | y <- x]

turnUltimateDialLeftToGetABigNumber :: Integer -> Integer -> Integer
turnUltimateDialLeftToGetABigNumber startingNumber rotationsLeft =
  (((startingNumber + rotationsLeft) * rotationsLeft) ^ ultimateAnswer) + _decryptSecretSeed

turnUltimateDialRightToGetASmallNumber :: Integer -> Integer -> Integer
turnUltimateDialRightToGetASmallNumber rotationsRight bigNumber = do
  let scrunched = take overHexed (show bigNumber)
  let packed = drop overHexed (show bigNumber)
  let packed1 = take ultimateWindageAdjustment packed
  let packed2 = drop ultimateWindageAdjustment packed
  let packed1Letters = _convertNumberToLetters (read packed1 :: Integer)
  let packed2Letters = _convertNumberToLetters (read packed2 :: Integer)
  let offset1 = _unpackToOffsetList packed1Letters
  let offset2 = _unpackToOffsetList packed2Letters
  let wholeValues = _unScrunchBinary (read scrunched :: Int)
  let wholeValues1 = _continueTurningLeftUntilClickIsHeard wholeValues offset2
  let wholeValues2 = _continueTurningLeftUntilClickIsHeard wholeValues1 offset1
  read $ foldl (\acc x -> acc ++ show x) "" wholeValues2 :: Integer

turnUltimateDialLeftUntilTheMessageAppears :: Integer -> Integer -> String
turnUltimateDialLeftUntilTheMessageAppears rotationsLeft smallerNumber = do
  let anIdiotsLuggageCombination = rotationsLeft
  let textNumber = show smallerNumber
  let order = [x * 4 | x <- [0..(length textNumber - 1)], x * 4 < (length textNumber - 1)]
  let y = [(read (take 4 (drop x textNumber)) :: Integer) - anIdiotsLuggageCombination | x <- order]
  let z = [ fromInteger x `shiftR` bitesShifted | x <- y]
  [if x == courseCorrection then chr (x `div` bassDecimal) else chr x | x <- z]



{--------------------------------

ASCII 3D Art

----------------------------------}

duplicateChar :: Char -> Int -> String
duplicateChar c x = [c | _<- [0..x]]

_createASCIIBoxWalls :: Int -> String
_createASCIIBoxWalls x = duplicateChar ' ' 4 ++ "|" ++ duplicateChar ' ' x ++ "|"

_createASCIIBoxBottom :: Int -> String
_createASCIIBoxBottom x = duplicateChar ' ' 5 ++ duplicateChar '-' x

_createASCIITopDepth :: Int -> Int -> Int -> String
_createASCIITopDepth offset x z = duplicateChar ' ' (5 + offset) ++ "/" ++ duplicateChar ' ' x ++ "/" ++ duplicateChar ' ' (z - offset - 2) ++ "|"

_createASCIIXTop :: Int -> Int -> Int -> String
_createASCIIXTop offset x z = duplicateChar ' ' (5 + offset) ++ "/" ++ duplicateChar '_' x ++ "/" ++ duplicateChar ' ' (z - offset - 2) ++ "|"

_createASCIIBoxTop :: Int -> String
_createASCIIBoxTop x = duplicateChar ' ' 5 ++ duplicateChar '_' x

createASCIIBox :: Int -> Int -> Int -> [String]
createASCIIBox xWidth yHeight zDepth
  | xWidth < 2 = error "Must have a width larger than 2"
  | yHeight < 2 = error "Must have a height larger than 2"
  | zDepth  < 1 = error "Must have a depth larger than 1"
  | zDepth > yHeight = error "zDepth cannot be larger than yHeight"
  | otherwise = do
    let realXWidth = xWidth * 3
    [duplicateChar ' ' (zDepth) ++ _createASCIIBoxTop realXWidth] ++
      [_createASCIITopDepth x realXWidth zDepth | x <- reverse [1..zDepth - 1]] ++
      [_createASCIIXTop 0 realXWidth (zDepth)] ++
      [] ++ (if yHeight - zDepth == 0 then [] else [_createASCIIBoxWalls realXWidth ++ duplicateChar ' ' (zDepth - 1) ++ "|" | _ <- reverse [0..(yHeight - zDepth)]]) ++
      [_createASCIIBoxWalls realXWidth ++ duplicateChar ' ' x ++ "/" | x <- reverse [0..zDepth - 1]] ++
      [_createASCIIBoxBottom realXWidth]
