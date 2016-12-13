module Main where

import Data.Maybe

version :: String
version = "0.0.1"

data TimeLog = TimeLog { records :: Records
                       , current :: Maybe Record
                       } deriving (Show, Eq)

type Records = [Record]
data Record = Record { recordNum :: String
                     , inTime :: Time
                     , outTime :: Maybe Time
                     , description :: Maybe String
                     , billable :: Maybe Bool
                     } deriving (Show, Eq)

type Time = Rational
type Date = Rational

main :: IO ()
main = do
  putStrLn $ "Timelogger v" ++ version
  putStrLn "Press \"h\" for help\n"
  mainLoop $ Just $ TimeLog [] Nothing

mainLoop :: Maybe TimeLog -> IO ()
mainLoop (Just timeLog) = do
  putStr "> "
  command <- getLine
  newLog <- handleCommand timeLog $ head command
  return newLog >>= mainLoop
mainLoop Nothing = return ()

handleCommand :: TimeLog -> Char -> IO (Maybe TimeLog)
handleCommand _ 'q' = return Nothing
handleCommand timeLog 'c' = handleClockInOut timeLog 123
handleCommand timeLog 'l' = printLog timeLog
handleCommand timeLog cmd = do
  putStrLn $ "Invalid command: " ++ [cmd]
  return $ Just timeLog

handleClockInOut :: TimeLog -> Date -> IO (Maybe TimeLog)
handleClockInOut timeLog date = do
  newLog <- if (clockedIn timeLog) then (clockOut timeLog) else (clockIn timeLog)
  return $ Just newLog

clockedIn :: TimeLog -> Bool
clockedIn timeLog = isJust (current timeLog)

clockIn :: TimeLog -> IO TimeLog
clockIn timeLog = do
  num <- promptRecordNum
  return $ TimeLog (records timeLog) (Just $ Record num 123 Nothing Nothing Nothing)

clockOut :: TimeLog -> IO TimeLog
clockOut timeLog = do
  desc <- promptDescription
  bill <- promptBillable
  let curr = fromJust $ current timeLog
      newRecord = Record (recordNum curr) (inTime curr) (Just 124) (Just desc) (Just bill)
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
