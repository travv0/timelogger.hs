module Main where

import Data.Maybe
import Data.Time
import System.IO

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
  newLog <- handleCommand day timeLog command
  return newLog >>= (mainLoop day)
mainLoop _ Nothing = return ()

printPrompt :: TimeLog -> Day -> IO ()
printPrompt timeLog day = do
  let curr = current timeLog
  putStrLn $ "Current date: " ++ (formatTime defaultTimeLocale "%D" day) ++ printCurrInfo curr
  currentTime <- getCurrentTime
  putStr $ (formatTime defaultTimeLocale "%R" currentTime) ++ "> "
  hFlush stdout

printCurrInfo :: Maybe Record -> String
printCurrInfo (Just curr) =
  " (Current: " ++ (recordNum curr) ++ ")"
printCurrInfo Nothing = ""

handleCommand :: Day -> TimeLog -> String -> IO (Maybe TimeLog)
handleCommand _ timeLog [] = return $ Just timeLog
handleCommand _ _ ('q':_) = return Nothing
handleCommand day timeLog ('c':_) = do
  handleClockInOut timeLog day
handleCommand day timeLog ('l':_) = printLog timeLog
handleCommand _ timeLog cmd = do
  putStrLn $ "Invalid command: " ++ cmd
  return $ Just timeLog

handleClockInOut :: TimeLog -> Day -> IO (Maybe TimeLog)
handleClockInOut timeLog day = do
  newLog <- if (clockedIn timeLog) then (clockOut timeLog) else (clockIn timeLog)
  return $ Just newLog

clockedIn :: TimeLog -> Bool
clockedIn timeLog = isJust (current timeLog)

clockIn :: TimeLog -> IO TimeLog
clockIn timeLog = do
  num <- prompt "Enter item ID: "
  currentTime <- getCurrentTime
  return $ TimeLog (records timeLog) (Just $ Record num currentTime Nothing Nothing Nothing)

clockOut :: TimeLog -> IO TimeLog
clockOut timeLog = do
  desc <- prompt "Enter a description of what you worked on: "
  bill <- promptYN "Was this work billable?"
  currentTime <- getCurrentTime
  let curr = fromJust $ current timeLog
      newRecord = Record (recordNum curr) (inTime curr) (Just currentTime) (Just desc) (Just bill)
  return $ TimeLog (newRecord : records timeLog) Nothing

prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

promptYN :: String -> IO Bool
promptYN s = do
  putStr $ s ++ " (y or n) "
  hFlush stdout
  getLine >>= readYorN

readYorN :: String -> IO Bool
readYorN "y" = return True
readYorN "n" = return False
readYorN _ = do
  putStr "Please type \"y\" for yes or \"n\" for no. "
  getLine >>= readYorN

printLog :: TimeLog -> IO (Maybe TimeLog)
printLog timeLog = do
  putStrLn $ show timeLog
  return $ Just timeLog
