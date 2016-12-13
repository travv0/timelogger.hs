module Main where

import Data.Maybe
import Data.Time

version :: String
version = "0.0.1"

data TimeLog = TimeLog { records :: Records
                       , current :: Maybe Record
                       } deriving (Show, Eq)

type Records = [Record]
data Record = Record { recordNum :: String
                     , inTime :: UTCTime
                     , outTime :: Maybe UTCTime
                     , description :: Maybe String
                     , billable :: Maybe Bool
                     } deriving (Show, Eq)

main :: IO ()
main = do
  putStrLn $ "Timelogger v" ++ version
  putStrLn "Press \"h\" for help\n"
  currentTime <- getCurrentTime
  let currentDay = utctDay currentTime
  mainLoop currentDay (Just $ TimeLog [] Nothing)

mainLoop :: Day -> Maybe TimeLog -> IO ()
mainLoop day (Just timeLog) = do
  printPrompt timeLog day
  command <- getLine
  newLog <- handleCommand day timeLog $ head command
  return newLog >>= (mainLoop day)
mainLoop _ Nothing = return ()

printPrompt :: TimeLog -> Day -> IO ()
printPrompt timeLog day = do
  let curr = current timeLog
  putStrLn $ "Current date: " ++ (formatTime defaultTimeLocale "%D" day) ++ printCurrInfo curr
  currentTime <- getCurrentTime
  putStr $ (formatTime defaultTimeLocale "%R" currentTime) ++ "> "

printCurrInfo :: Maybe Record -> String
printCurrInfo (Just curr) =
  " (Current: " ++ (recordNum curr) ++ ")"
printCurrInfo Nothing = ""

handleCommand :: Day -> TimeLog -> Char -> IO (Maybe TimeLog)
handleCommand _ _ 'q' = return Nothing
handleCommand day timeLog 'c' = do
  handleClockInOut timeLog day
handleCommand day timeLog 'l' = printLog timeLog
handleCommand _ timeLog cmd = do
  putStrLn $ "Invalid command: " ++ [cmd]
  return $ Just timeLog

handleClockInOut :: TimeLog -> Day -> IO (Maybe TimeLog)
handleClockInOut timeLog day = do
  newLog <- if (clockedIn timeLog) then (clockOut timeLog) else (clockIn timeLog)
  return $ Just newLog

clockedIn :: TimeLog -> Bool
clockedIn timeLog = isJust (current timeLog)

clockIn :: TimeLog -> IO TimeLog
clockIn timeLog = do
  num <- promptRecordNum
  currentTime <- getCurrentTime
  return $ TimeLog (records timeLog) (Just $ Record num currentTime Nothing Nothing Nothing)

clockOut :: TimeLog -> IO TimeLog
clockOut timeLog = do
  desc <- promptDescription
  bill <- promptBillable
  currentTime <- getCurrentTime
  let curr = fromJust $ current timeLog
      newRecord = Record (recordNum curr) (inTime curr) (Just currentTime) (Just desc) (Just bill)
  return $ TimeLog (newRecord : records timeLog) Nothing

promptRecordNum :: IO String
promptRecordNum = do
  putStr "Enter item ID: "
  getLine

promptDescription :: IO String
promptDescription = do
  putStr "Enter a description of what you worked on: "
  getLine

promptBillable :: IO Bool
promptBillable = do
  putStr "Was this work billable? (y or n) "
  getLine >>= readYorN

readYorN :: String -> IO Bool
readYorN "y" = return True
readYorN "n" = return False
readYorN _ = do
  putStrLn "Please type \"y\" for yes or \"n\" for no."
  getLine >>= readYorN

printLog :: TimeLog -> IO (Maybe TimeLog)
printLog timeLog = do
  putStrLn $ show timeLog
  return $ Just timeLog
