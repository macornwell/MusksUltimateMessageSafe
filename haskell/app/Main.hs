module Main where

import Lib
import qualified Control.Concurrent as Concurrent


main :: IO ()
main = do
  let dramaticWaitInSeconds = 2
  let microSecondsInASecond = 1000000
  mapM_ putStrLn (createASCIIBox 7 20 2)
  putStrLn "Welcome to Elon Musk's Ultimate Message Safe. Only Elon may enter."
  putStrLn "The safe requires the following four (4) inputs:"
  putStrLn ""
  putStrLn "    [STARTING_NUMBER]     The number to start the safe on."
  putStrLn "    [TURNS_LEFT]          The number of turns left to turn the dial."
  putStrLn "    [TURNS_RIGHT]         The number of turns right to turn the dial."
  putStrLn "    [TURNS_LEFT]          The number of turns left to turn the dial to get the message."
  putStrLn ""
  putStrLn "What number should the safe start on?"
  line <- getLine
  let startingNumber = read line :: Integer
  putStrLn "How many turns left?"
  line <- getLine
  let turnsLeft = read line :: Integer
  putStrLn "How many turns right?"
  line <- getLine
  let turnsRight = read line :: Integer
  putStrLn "How many turns left?"
  line <- getLine
  let turnsLeft2 = read line :: Integer

  putStrLn $ "You set the safe on number ( " ++ show startingNumber ++ " )."
  Concurrent.threadDelay $ dramaticWaitInSeconds * microSecondsInASecond
  putStrLn $ "You start turning the safe ( " ++ show turnsLeft ++ " ) turns to the left, slowly."
  Concurrent.threadDelay $ dramaticWaitInSeconds * microSecondsInASecond
  putStrLn "You lose grip on the dial and it continues turning on its own."
  Concurrent.threadDelay $ dramaticWaitInSeconds * microSecondsInASecond
  let bigNumber = turnUltimateDialLeftToGetABigNumber startingNumber turnsLeft
  putStrLn $ "Something kicks in and boosts the spinning of the combination lock. The safe spins out of control to " ++ show bigNumber ++ "."
  Concurrent.threadDelay $ dramaticWaitInSeconds * microSecondsInASecond
  let smallNumber = turnUltimateDialRightToGetASmallNumber turnsRight bigNumber
  putStrLn $ "The safe turns ( " ++ show turnsRight ++ " ) rotations to the right and settles on " ++ show smallNumber ++ "."
  Concurrent.threadDelay $ dramaticWaitInSeconds * microSecondsInASecond
  putStrLn $ "You turn the safe left ( " ++ show turnsLeft2 ++ " ) times and the following message appears...  "
  let message = turnUltimateDialLeftUntilTheMessageAppears turnsLeft2 smallNumber
  putStrLn ""
  putStrLn message
