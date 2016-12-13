module Main where

import Control.Monad.State.Lazy
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

type Time = Integer
type Date = Integer

commands :: [(String, TimeLog -> Date -> IO (Maybe (State TimeLog TimeLog), Date))]
commands = [ ("c", handleClockInOut)
           -- , ("l", dummyFunc) --printLog)
           -- , ("d", dummyFunc) --changeDate)
           , ("q", quit)
           ]

main :: IO ()
main = do
  putStrLn $ "Timelogger v" ++ version
  putStrLn "Press \"h\" for help\n"
  mainLoop $ TimeLog [] Nothing

mainLoop :: TimeLog -> IO ()
mainLoop timeLog = do
  putStr "> "
  command <- getLine
  let (Just action) = lookup command commands
  newLog <- action timeLog
  when (isJust newLog)
    $ mainLoop timeLog

handleClockInOut :: TimeLog -> Date -> IO (Maybe (State TimeLog TimeLog), Date)
handleClockInOut timeLog = do
  let action = if (clockedIn timeLog) then clockIn else clockOut
  let newLog = TimeLog (records timeLog) (Just action)
  return $ Just (runState)
  -- if (isJust $ current timeLog)

clockedIn :: TimeLog -> Bool
clockedIn timeLog = isJust (current timeLog)

clockIn :: IO Record
clockIn = do
  recordNum <- promptRecordNum
  return $ Record recordNum 123 Nothing Nothing Nothing

clockOut :: IO Record
clockOut = do
  recordNum <- promptRecordNum
  return $ Record recordNum 123 Nothing Nothing Nothing

promptRecordNum :: IO String
promptRecordNum = do
  putStr "Enter item ID: "
  getLine

quit :: TimeLog -> Date -> IO (Maybe (State TimeLog TimeLog), Date)
quit _ _ = return Nothing
