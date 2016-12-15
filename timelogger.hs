module Main where

import Data.Maybe
import Data.Time
import Data.List
import System.IO
import System.Directory
import Control.Monad
import Control.Exception

version :: String
version = "0.0.1"

data TimeLog = TimeLog { records :: Records
                       , current :: Maybe Record
                       } deriving (Read, Show, Eq)

type Records = [Record]
data Record = Record { recordNum :: String
                     , inTime :: LocalTime
                     , outTime :: Maybe LocalTime
                     , description :: Maybe String
                     , billable :: Maybe Bool
                     } deriving (Read, Show, Eq)

commands :: [(Char,Day -> TimeLog -> IO (Maybe (TimeLog,Day)))]
commands = [ ('q', quit)
           , ('c', initClockInOut)
           , ('C', initDelayedClockInOut)
           , ('d', changeDate)
           , ('l', printLog)
           , ('L', printTimeTable)
           , ('e', editTimeLog)
           , ('h', printHelp)
           , ('v', printVersionInfo)
           ]

main :: IO ()
main = do
  printVersion
  putStrLn "Press \"h\" for help"
  currentDay <- getToday
  timeLog <- loadTimeLog currentDay
  mainLoop currentDay $ Just timeLog

mainLoop :: Day -> Maybe TimeLog -> IO ()
mainLoop day (Just timeLog) = do
  printPrompt timeLog day
  result <- getLine >>= handleCommand day timeLog
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
  currentTime <- getLocalTime
  putStr $ (formatTime defaultTimeLocale "%R" currentTime) ++ "> "
  hFlush stdout

printCurrInfo :: Maybe Record -> IO ()
printCurrInfo (Just curr) = do
  currMinutes <- getMinutes curr
  putStr $ " (Current: " ++ (recordNum curr) ++ " - Minutes: " ++ show currMinutes ++ ")"
printCurrInfo Nothing = return ()

handleCommand :: Day -> TimeLog -> String -> IO (Maybe (TimeLog,Day))
handleCommand day timeLog [] = return $ Just (timeLog,day)
handleCommand day timeLog (cmd:_) = do
  let action = lookup cmd commands
  if null action
    then do
      putStrLn $ "Invalid command: " ++ [cmd]
      return $ Just (timeLog,day)
    else
      (fromJust action) day timeLog

quit :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
quit _ _ = return Nothing

initClockInOut :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
initClockInOut day timeLog = do
  currentTime <- getLocalTime
  newLog <- handleClockInOut timeLog currentTime day
  return $ Just (newLog,day)

initDelayedClockInOut :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
initDelayedClockInOut day timeLog = do
  inputtedTime <- prompt "How many minutes ago to clock in/out?"
  if isJust inputtedTime
    then do
      time <- handleInputtedTime (fromJust inputtedTime)
      newLog <- handleClockInOut timeLog time day
      return $ Just (newLog,day)
    else do
      putStrLn "Canceled"
      return $ Just (timeLog,day)

handleInputtedTime :: String -> IO LocalTime
handleInputtedTime s = do
  utcTime <- getCurrentTime
  zone <- getCurrentTimeZone
  let time = utctDayTime utcTime
      offset = secondsToDiffTime $ read s * 60
      newTime = UTCTime (utctDay utcTime) (time - offset)
  return $ utcToLocalTime zone newTime

printHelp :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
printHelp day timeLog = do
  putStrLn $ "Commands:\n" ++
    "c: clock in/out\n" ++
    "C: delayed clock in/out\n" ++
    "d: change date\n" ++
    "l: print log\n" ++
    "L: print timetable\n" ++
    "h: show this help\n" ++
    "v: show version information\n" ++
    "q: quit"
  return $ Just (timeLog,day)

printVersionInfo :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
printVersionInfo day timeLog = do
  printVersion
  printCredits
  return $ Just (timeLog,day)

printVersion :: IO ()
printVersion = putStrLn $ "Timelogger v" ++ version

printCredits :: IO ()
printCredits = putStrLn $ "Created by Travis"

changeDate :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
changeDate day timeLog = do
  newDay <- prompt "Enter new date in format MM/DD/YY (leave blank for today):"
  parsedDayE <- if isJust (newDay)
                then try $ parseTimeM True defaultTimeLocale "%-m/%-d/%-y" (fromJust newDay)
                     :: IO (Either IOError Day)
                else do today <- getToday
                        return $ Right today
  case parsedDayE of
    Left _ -> do
      putStrLn "Invalid date format. Expected MM/DD/YY."
      return $ Just (timeLog,day)
    Right parsedDay -> do
      newLog <- loadTimeLog parsedDay
      return $ Just (newLog,parsedDay)

-- TODO: handle bad input
editTimeLog :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
editTimeLog day timeLog = do
  printTimeLogList timeLog
  num <- prompt "Enter ID of record to change:"
  newLog <- if isJust num
            then editRecordInLog timeLog $ read $ fromJust num
            else return timeLog
  saveTimeLog day newLog
  return $ Just (newLog,day)

editRecordInLog :: TimeLog -> Int -> IO TimeLog
editRecordInLog timeLog n
  | n - 1 == length (records timeLog) = do
      newCurr <- editRecord $ fromJust (current timeLog)
      return $ TimeLog (records timeLog) $ Just newCurr
  | otherwise = do
      let rcds = (records timeLog)
      newRcd <- editRecord (rcds !! (n - 1))
      return $ TimeLog (replaceAtIndex (n - 1) newRcd rcds) (current timeLog)

-- TODO: handle bad input
editRecord :: Record -> IO Record
editRecord rcd = do
  printEditOptions rcd
  ind <- prompt "Enter ID of desired action:"
  case (read $ fromJust ind) :: Int of
    1 -> do
      num <- prompt "Enter new record number:"
      if isJust num
        then return $ Record (fromJust num) (inTime rcd) (outTime rcd) (description rcd) (billable rcd)
        else return rcd
    2 -> do
      time <- prompt $ "Enter new in-time (changing from " ++ formatTime defaultTimeLocale "%R" (inTime rcd) ++ "):"
      let date = formatTime defaultTimeLocale "%F" (inTime rcd)
      parsedTime <- parseTimeM True defaultTimeLocale "%F%R" $ date ++ fromJust time
      return $ Record (recordNum rcd) parsedTime (outTime rcd) (description rcd) (billable rcd)
    3 -> do
      time <- prompt $ "Enter new out-time (changing from " ++
        formatTime defaultTimeLocale "%R" (fromJust (outTime rcd)) ++ "):"
      let date = formatTime defaultTimeLocale "%F" $ fromJust (outTime rcd)
      parsedTime <- parseTimeM True defaultTimeLocale "%F%R" $ date ++ fromJust time
      return $ Record (recordNum rcd) (inTime rcd) (Just parsedTime) (description rcd) (billable rcd)
    4 -> do
      desc <- prompt "Enter new description:"
      if isJust desc
        then return $ Record (recordNum rcd) (inTime rcd) (outTime rcd) desc (billable rcd)
        else return rcd
    5 -> do
      bill <- promptYN "Was this work billable?"
      return $ Record (recordNum rcd) (inTime rcd) (outTime rcd) (description rcd) (Just bill)
    _ -> do
      putStrLn "Out of range"
      return rcd

printEditOptions :: Record -> IO ()
printEditOptions rcd = do
  putStrLn "1. Edit item number"
  putStrLn "2. Edit in-time"
  when (isJust (outTime rcd))
    $ putStrLn "3. Edit out-time"
  when (isJust (description rcd))
    $ putStrLn "4. Edit description"
  when (isJust (billable rcd))
    $ putStrLn "5. Edit billable"

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

printTimeLogList :: TimeLog -> IO ()
printTimeLogList timeLog = do
  let mCurr = (current timeLog)
      descCurr = if isJust mCurr
                 then let curr = fromJust mCurr
                          in [Record (recordNum curr) (inTime curr) Nothing (Just "Currently clocked in") Nothing]
                 else []
  _ <- sequence $ map putStrLn (zipWith (\num rcd ->
                                           show num ++ ". " ++ (recordNum rcd) ++ " - " ++
                                          fromJust (description rcd))
                                ([1..] :: [Int]) $ records timeLog ++ descCurr)
  return ()



handleClockInOut :: TimeLog -> LocalTime -> Day -> IO TimeLog
handleClockInOut timeLog time day = do
  currentDay <- getToday
  if (day == currentDay)
    then do
      newLog <- if (clockedIn timeLog) then (clockOut timeLog time) else (clockIn timeLog time)
      saveTimeLog day newLog
      return newLog
    else do
      putStrLn "You must be on today's date to clock in or out."
      return timeLog

clockedIn :: TimeLog -> Bool
clockedIn timeLog = isJust (current timeLog)

getToday :: IO Day
getToday = do
  currentTime <- getLocalTime
  return $ localDay currentTime

getLocalTime :: IO LocalTime
getLocalTime = do
  zonedTime <- getZonedTime
  return $ zonedTimeToLocalTime zonedTime

clockIn :: TimeLog -> LocalTime -> IO TimeLog
clockIn timeLog time = do
  num <- prompt "Enter item ID:"
  if (null num)
    then do
      putStrLn "Canceled"
      return timeLog
    else do
      putStrLn $ "Clocked in at " ++ formatTime defaultTimeLocale "%R" time
      return $ TimeLog (records timeLog) (Just $ Record (fromJust num) time Nothing Nothing Nothing)

clockOut :: TimeLog -> LocalTime -> IO TimeLog
clockOut timeLog time = do
  desc <- prompt "Enter a description of what you worked on:"
  if (null desc)
    then do
      putStrLn "Canceled"
      return timeLog
    else do
      bill <- promptYN "Was this work billable?"
      let curr = fromJust $ current timeLog
          newRecord = Record (recordNum curr) (inTime curr) (Just time) desc (Just bill)
      putStrLn $ "Clocked out at " ++ formatTime defaultTimeLocale "%R" time
      return $ TimeLog (records timeLog ++ [newRecord]) Nothing

prompt :: String -> IO (Maybe String)
prompt s = do
  putStr $ s ++ " "
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

seperatorLen :: Int
seperatorLen = 50

seperatorChar :: Char
seperatorChar = '-'

seperator :: String
seperator = replicate seperatorLen seperatorChar

printLog :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
printLog day timeLog = do
  putStrLn $ "\nTime log for " ++ show day
  let recs = records timeLog
  let ids = getRecordNums recs
  _ <- mapM (printRecordsForNum recs) ids
  putStrLn $ "\n" ++ seperator
  return $ Just (timeLog,day)

printTimeTable :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
printTimeTable day timeLog = do
  let singletonCurr = if isJust (current timeLog) then [fromJust (current timeLog)] else []
  _ <- mapM printTimeTableRow $ (records timeLog) ++ singletonCurr
  return $ Just (timeLog,day)

printTimeTableRow :: Record -> IO ()
printTimeTableRow rcd = do
  let out = if isJust (outTime rcd)
            then formatTime defaultTimeLocale "%R" (fromJust (outTime rcd))
            else "....."
      desc = if isJust (description rcd)
             then fromJust (description rcd)
             else "Currently clocked in"
  putStrLn $ formatTime defaultTimeLocale "%R" (inTime rcd) ++ "-" ++
    out ++ "   " ++ (recordNum rcd) ++ " - " ++ desc

getRecordNums :: Records -> [String]
getRecordNums recs = nub $ map (\rcd -> recordNum rcd) recs

printRecordsForNum :: Records -> String -> IO ()
printRecordsForNum recs num = do
  putStrLn $ "\n" ++ seperator ++ "\n"
  putStrLn $ num ++ ":"
  _ <- mapM printRecord (filter (\rcd -> ((recordNum rcd) == num)) recs)
  return ()

printRecord :: Record -> IO ()
printRecord rcd = do
  mins <- getMinutes rcd
  putStrLn $ "\nDescription: " ++ fromJust (description rcd)
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
      zone <- getCurrentTimeZone
      return $ getMinuteDifference currentTime $ localTimeToUTC zone (inTime record)
  | otherwise = do
      zone <- getCurrentTimeZone
      return $ getMinuteDifference (localTimeToUTC zone (fromJust $ outTime record))
                                   (localTimeToUTC zone (inTime record))

getMinuteDifference :: UTCTime -> UTCTime -> Int
getMinuteDifference a b = round $ (realToFrac $ diffUTCTime a b :: Float) / 60

getTotalMinutes :: TimeLog -> IO Int
getTotalMinutes timeLog = foldM (\total record ->
                                   liftM2 (+) (return total) (getMinutes record)) 0
                          (records timeLog ++ curr)
                          where curr = if (isJust (current timeLog))
                                          then [fromJust (current timeLog)]
                                          else []
