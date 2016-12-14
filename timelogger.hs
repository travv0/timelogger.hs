module Main where

import Data.Maybe
import Data.Time
import Data.List
import System.IO
import System.Directory
import Control.Monad

version :: String
version = "0.0.1"

data TimeLog = TimeLog { records :: Records
                       , current :: Maybe Record
                       } deriving (Read, Show, Eq)

type Records = [Record]
data Record = Record { recordNum :: String
                     , inTime :: UTCTime
                     , outTime :: Maybe UTCTime
                     , description :: Maybe String
                     , billable :: Maybe Bool
                     } deriving (Read, Show, Eq)

main :: IO ()
main = do
  putStrLn $ "Timelogger v" ++ version
  putStrLn "Press \"h\" for help\n"
  currentTime <- getCurrentTime
  let currentDay = utctDay currentTime
  timeLog <- loadTimeLog currentDay
  mainLoop currentDay $ Just timeLog

mainLoop :: Day -> Maybe TimeLog -> IO ()
mainLoop day (Just timeLog) = do
  printPrompt timeLog day
  command <- getLine
  result <- handleCommand day timeLog command
  if (null result)
    then return ()
    else do
      let newDay = snd $ fromJust result
          newLog = fst $ fromJust result
      mainLoop newDay (Just newLog)
mainLoop _ Nothing = return ()

printPrompt :: TimeLog -> Day -> IO ()
printPrompt timeLog day = do
  let curr = current timeLog
  totalMins <- getTotalMinutes timeLog
  putStr $ "\nCurrent date: " ++ (formatTime defaultTimeLocale "%D" day) ++
    " - Total minutes: " ++ (show totalMins)
  printCurrInfo curr
  putStr "\n"
  currentTime <- getCurrentTime
  putStr $ (formatTime defaultTimeLocale "%R" currentTime) ++ "> "
  hFlush stdout

printCurrInfo :: Maybe Record -> IO ()
printCurrInfo (Just curr) = do
  currMinutes <- getMinutes curr
  putStr $ " (Current: " ++ (recordNum curr) ++ " - Minutes: " ++ show currMinutes ++ ")"
printCurrInfo Nothing = return ()

handleCommand :: Day -> TimeLog -> String -> IO (Maybe (TimeLog,Day))
handleCommand day timeLog [] = return $ Just (timeLog,day)
handleCommand _ _ ('q':_) = return Nothing
handleCommand day timeLog ('c':_) = do
  newLog <- handleClockInOut timeLog day
  return $ Just (newLog,day)
handleCommand _ _ ('d':_) = do
  newDay <- prompt "Enter new date: "
  parsedDay <- parseTimeM True defaultTimeLocale "%D" (fromJust newDay)
  newLog <- loadTimeLog parsedDay
  return $ Just (newLog,parsedDay)
handleCommand day timeLog ('l':_) = do
  printLog day timeLog
  return $ Just (timeLog,day)
handleCommand day timeLog cmd = do
  putStrLn $ "Invalid command: " ++ cmd
  return $ Just (timeLog,day)

handleClockInOut :: TimeLog -> Day -> IO TimeLog
handleClockInOut timeLog day = do
  currentTime <- getCurrentTime
  let currentDay = utctDay currentTime
  if (day == currentDay)
    then do
      newLog <- if (clockedIn timeLog) then (clockOut timeLog) else (clockIn timeLog)
      saveTimeLog day newLog
      return newLog
    else do
      putStrLn "You must be on today's date to clock in or out."
      return timeLog

clockedIn :: TimeLog -> Bool
clockedIn timeLog = isJust (current timeLog)

clockIn :: TimeLog -> IO TimeLog
clockIn timeLog = do
  num <- prompt "Enter item ID: "
  if (null num)
    then do
      putStrLn "Canceled"
      return timeLog
    else do
      currentTime <- getCurrentTime
      return $ TimeLog (records timeLog) (Just $ Record (fromJust num) currentTime Nothing Nothing Nothing)

clockOut :: TimeLog -> IO TimeLog
clockOut timeLog = do
  desc <- prompt "Enter a description of what you worked on: "
  if (null desc)
    then do
      putStrLn "Canceled"
      return timeLog
    else do
      bill <- promptYN "Was this work billable?"
      currentTime <- getCurrentTime
      let curr = fromJust $ current timeLog
          newRecord = Record (recordNum curr) (inTime curr) (Just currentTime) desc (Just bill)
      return $ TimeLog (newRecord : records timeLog) Nothing

prompt :: String -> IO (Maybe String)
prompt s = do
  putStr s
  hFlush stdout
  response <- getLine
  if (null response)
    then return Nothing
    else return $ Just response

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

printLog :: Day -> TimeLog -> IO ()
printLog day timeLog = do
  putStrLn $ "\nTime log for " ++ show day
  let recs = records timeLog
  let ids = getRecordNums recs
  _ <- mapM (printRecordsForNum recs) ids
  putStrLn "\n-------------------------------------"

getRecordNums :: Records -> [String]
getRecordNums recs = nub $ map (\rcd -> recordNum rcd) recs

printRecordsForNum :: Records -> String -> IO ()
printRecordsForNum recs num = do
  putStrLn "\n-------------------------------------\n"
  putStrLn $ num ++ ":\n"
  _ <- mapM printRecord (filter (\rcd -> ((recordNum rcd) == num)) recs)
  return ()

printRecord :: Record -> IO ()
printRecord rcd = do
  mins <- getMinutes rcd
  putStrLn $ "Description: " ++ fromJust (description rcd)
  putStrLn $ show mins ++ " Minutes, " ++  if fromJust (billable rcd)
                                           then "Billable"
                                           else "Non-billable"

formatFileName :: Day -> String
formatFileName day = (formatTime defaultTimeLocale "%_Y%m%d" day)

saveTimeLog :: Day -> TimeLog -> IO ()
saveTimeLog day timeLog =
  writeFile (formatFileName day) $ show timeLog

loadTimeLog :: Day -> IO TimeLog
loadTimeLog day = do
  let fileName = formatFileName day
  fileExists <- doesFileExist fileName
  if fileExists
    then do
      timeLog <- readFile (formatFileName day)
      return $ (read timeLog :: TimeLog)
    else
      return $ TimeLog [] Nothing


getMinutes :: Record -> IO Int
getMinutes record
  | null (outTime record) = do
      currentTime <- getCurrentTime
      return $ getMinuteDifference currentTime (inTime record)
  | otherwise = return $ getMinuteDifference (fromJust $ outTime record) (inTime record)

getMinuteDifference :: UTCTime -> UTCTime -> Int
getMinuteDifference a b = round $ (realToFrac $ diffUTCTime a b :: Float) / 60

getTotalMinutes :: TimeLog -> IO Int
getTotalMinutes timeLog = foldM (\total record ->
                                   liftM2 (+) (return total) (getMinutes record)) 0
                          (records timeLog ++ curr)
                          where curr = if (isJust (current timeLog))
                                          then [fromJust (current timeLog)]
                                          else []
