module Main where

import Data.Maybe
import Data.Time
import Data.List
import System.IO
import System.Directory
import Control.Monad
import Control.Exception

version :: String
version = "1.4.1"

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

dataFilePath :: IO FilePath
dataFilePath = do
  home <- getHomeDirectory
  return $ home ++ "/Documents/timelogger.hs/data/"

commands :: [(Char,Day -> TimeLog -> IO (Maybe (TimeLog,Day)))]
commands = [ ('q', quit)
           , ('c', initClockInOut)
           , ('C', initDelayedClockInOut)
           , ('d', changeDate)
           , ('l', printLog)
           , ('L', printTimeTable)
           , ('e', editTimeLog)
           , ('f', fixRecordNum)
           , ('h', printHelp)
           , ('v', printVersionInfo)
           ]

main :: IO ()
main = do
  printVersion
  dataPath <- dataFilePath
  createDirectoryIfMissing True dataPath
  putStrLn "Press \"h\" for help"
  currentDay <- getToday
  timeLog <- loadTimeLog currentDay
  mainLoop currentDay $ Just timeLog

mainLoop :: Day -> Maybe TimeLog -> IO ()
mainLoop day (Just timeLog) = do
  printPrompt timeLog day
  result <- getLine >>= handleCommand day timeLog
  case result of
    Just r -> do
      let newDay = snd r
          newLog = fst r
      mainLoop newDay (Just newLog)
    Nothing -> return ()
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
  case lookup cmd commands of
    Just action -> action day timeLog
    Nothing -> do
      putStrLn $ "Invalid command: " ++ [cmd]
      return $ Just (timeLog,day)

quit :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
quit _ _ = return Nothing

canceledMessage :: String
canceledMessage = "Canceled."

initClockInOut :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
initClockInOut day timeLog = do
  currentTime <- getLocalTime
  newLog <- handleClockInOut timeLog currentTime day
  return $ Just (newLog,day)

initDelayedClockInOut :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
initDelayedClockInOut day timeLog = do
  inputtedTime <- prompt "How many minutes ago to clock in/out? (Empty cancels)"
  case inputtedTime of
    Just t -> do
      time <- handleInputtedMinutes (LocalTime day (TimeOfDay 0 0 0)) t
      case time of
        Just t2 -> do
          newLog <- handleClockInOut timeLog t2 day
          return $ Just (newLog,day)
        Nothing -> return $ Just (timeLog,day)
    Nothing -> do
      putStrLn canceledMessage
      return $ Just (timeLog,day)

handleInputtedMinutes :: LocalTime -> String -> IO (Maybe LocalTime)
handleInputtedMinutes day s = do
  utcTime <- getCurrentTime
  zone <- getCurrentTimeZone
  let currTime = utctDayTime utcTime
  newTime <- parseTimeInput day s
  case newTime of
    Just t -> return $ Just t
    Nothing -> do
      case reads s of
        [(time,_)] -> do
          let offset = secondsToDiffTime $ time * 60
              adjustedTime = UTCTime (utctDay utcTime) (currTime - offset)
          return $ Just $ utcToLocalTime zone adjustedTime
        _ -> do
          putStrLn "Invalid input."
          return Nothing

printHelp :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
printHelp day timeLog = do
  putStrLn $ "Commands:\n" ++
    "c: clock in/out\n" ++
    "C: delayed clock in/out\n" ++
    "d: change date\n" ++
    "l: print log\n" ++
    "L: print timetable\n" ++
    "e: edit record\n" ++
    "f: edit record number for all records with given number\n" ++
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
  newDay <- prompt "Enter new date in format MM/DD/YY, MM/DD, or DD (leave blank for today):"
  parsedDayE <- if isJust (newDay)
                then parseDate (fromJust newDay) day
                else do today <- getToday
                        return $ Right today
  case parsedDayE of
    Left _ -> do
      putStrLn "Invalid date format. Expected MM/DD/YY, MM/DD, or DD."
      return $ Just (timeLog,day)
    Right parsedDay -> do
      newLog <- loadTimeLog parsedDay
      return $ Just (newLog,parsedDay)

parseDate :: String -> Day -> IO (Either IOError Day)
parseDate s today
  | (length . filter (=='/')) s == 2 = try $ parseTimeM True defaultTimeLocale "%-m/%-d/%-y" s
  | (length . filter (=='/')) s == 1 = try $ parseTimeM True defaultTimeLocale "%-m/%-d/%-y"
                                       (s ++ formatTime defaultTimeLocale "/%y" today)
  | (length . filter (=='-')) s == 2 = try $ parseTimeM True defaultTimeLocale "%-m-%-d-%-y" s
  | (length . filter (=='-')) s == 1 = try $ parseTimeM True defaultTimeLocale "%-m-%-d-%-y"
                                       (s ++ formatTime defaultTimeLocale "-%y" today)
  | otherwise = try $ parseTimeM True defaultTimeLocale "%-d/%-m/%-y"
                (s ++ formatTime defaultTimeLocale "/%m/%y" today)

editTimeLog :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
editTimeLog day timeLog = do
  case timeLog of
    TimeLog [] Nothing -> do
      putStrLn "No records for this day."
      return $ Just (timeLog,day)
    _ -> do
      printTimeLogList timeLog
      num <- prompt "Enter ID of record to change (Empty cancels):"
      newLog <- if isJust num
                then do
                  editRecordInLog timeLog (reads $ fromJust num :: [(Int,String)])
                else do
                  putStrLn canceledMessage
                  return timeLog
      saveTimeLog day newLog
      return $ Just (newLog,day)

editRecordInLog :: TimeLog -> [(Int,String)] -> IO TimeLog
editRecordInLog timeLog [(n,_)]
  | n - 1 == length (records timeLog) && clockedIn timeLog = do
      newCurr <- editRecord $ fromJust (current timeLog)
      return $ TimeLog (records timeLog) $ Just newCurr
  | n <= length (records timeLog) && n > 0 = do
      let rcds = (records timeLog)
      newRcd <- editRecord (rcds !! (n - 1))
      return $ TimeLog (sortRecords (replaceAtIndex (n - 1) newRcd rcds)) (current timeLog)
  | otherwise = do
      putStrLn "Out of range"
      return timeLog
editRecordInLog timeLog _ = do
  putStrLn "Invalid input"
  return timeLog

sortRecords :: Records -> Records
sortRecords rcds = sortBy sortInTime rcds
  where sortInTime a b
          | (inTime a) > (inTime b) = GT
          | (inTime a) < (inTime b) = LT
          | otherwise = EQ

editRecord :: Record -> IO Record
editRecord rcd = do
  printEditOptions rcd
  ind <- prompt "Enter ID of desired action (Empty cancels):"
  if isJust ind
    then handleEditID rcd (reads $ fromJust ind :: [(Int,String)])
    else do
      putStrLn canceledMessage
      return rcd

handleEditID :: Record -> [(Int,String)] -> IO Record
handleEditID rcd [(ind,_)] = do
  if isJust (outTime rcd) || ind < 3 && ind > 0
    then
    case ind of
      1 -> editRecordNum rcd
      2 -> editInTime rcd
      3 -> editOutTime rcd
      4 -> editDescription rcd
      5 -> editBillable rcd
      _ -> do
        putStrLn "Out of range"
        return rcd
    else do
      putStrLn "Out of range"
      return rcd
handleEditID rcd _ = do
  putStrLn canceledMessage
  return rcd

editRecordNum :: Record -> IO Record
editRecordNum rcd = do
  num <- prompt "Enter new record number (Empty cancels):"
  case num of
    Just n ->
      return $ Record n (inTime rcd) (outTime rcd) (description rcd) (billable rcd)
    Nothing -> do
      putStrLn canceledMessage
      return rcd

parseTimeInput :: LocalTime -> String -> IO (Maybe LocalTime)
parseTimeInput day time = do
  let date = formatTime defaultTimeLocale "%F " day
  parsedTime <- try $ parseTimeM True defaultTimeLocale "%F %k:%M" $ date ++ time :: IO (Either IOError LocalTime)
  case parsedTime of
    Left _ -> return Nothing
    Right newTime -> return $ Just newTime

editInTime :: Record -> IO Record
editInTime rcd = do
  time <- prompt $ "Enter new in-time (changing from " ++ formatTime defaultTimeLocale "%R" (inTime rcd) ++ "):"
  parsedTime <- if isJust time
                then parseTimeInput (inTime rcd) $ fromJust time
                else return Nothing
  case parsedTime of
    Just t -> return $ Record (recordNum rcd) t (outTime rcd) (description rcd) (billable rcd)
    Nothing -> do
      putStrLn "Invalid input.  Expected time in format HH:MM."
      return rcd

editOutTime :: Record -> IO Record
editOutTime rcd = do
  time <- prompt $ "Enter new out-time (changing from " ++
    formatTime defaultTimeLocale "%R" (fromJust $ outTime rcd) ++ "):"
  case time of
    Just nt -> do
      parsedTime <- parseTimeInput (fromJust $ outTime rcd) nt
      case parsedTime of
        Just pt ->
          return $ Record (recordNum rcd) (inTime rcd) (Just pt) (description rcd) (billable rcd)
        Nothing -> do
          putStrLn "Invalid input.  Expected time in format HH:MM."
          return rcd
    Nothing -> do
      putStrLn "Invalid input.  Expected time in format HH:MM."
      return rcd

editDescription :: Record -> IO Record
editDescription rcd = do
  desc <- prompt "Enter new description (Empty cancels):"
  if isJust desc
    then return $ Record (recordNum rcd) (inTime rcd) (outTime rcd) desc (billable rcd)
    else do
      putStrLn canceledMessage
      return rcd

editBillable :: Record -> IO Record
editBillable rcd = do
  bill <- promptYN "Was this work billable?"
  return $ Record (recordNum rcd) (inTime rcd) (outTime rcd) (description rcd) (Just bill)

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

fixRecordNum :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
fixRecordNum day timeLog
  | length (records timeLog) == 0 = do
      putStrLn "Can't fix record number on day with no records."
      return $ Just (timeLog, day)
  | otherwise = do
      _ <- sequence $ fmap putStrLn $ zipWith (\n rcd -> show n ++ ". " ++ rcd)
           ([1..] :: [Int]) $ getRecordNums (records timeLog)
      ind <- prompt "Enter ID (Empty cancels):"
      case ind of
        Just i -> case reads i of
                    [(n,_)] -> do
                      newLog <- fixRecordNumInLog n timeLog
                      saveTimeLog day newLog
                      return $ Just (newLog, day)
                    _ -> do
                      putStrLn "Invalid input."
                      return $ Just (timeLog, day)
        Nothing -> do
          putStrLn canceledMessage
          return $ Just (timeLog, day)

fixRecordNumInLog :: Int -> TimeLog -> IO TimeLog
fixRecordNumInLog n timeLog
  | n <= length (records timeLog) && n > 0 = do
      let oldNum = (recordNum $ (records timeLog) !! (n - 1))
      newNum <- prompt "Enter new item number (Empty cancels):"
      case newNum of
        Just num -> return $ replaceAllRecordNums timeLog oldNum num
        Nothing -> do
          putStrLn canceledMessage
          return timeLog
  | otherwise = do
      putStrLn "Invalid ID."
      return timeLog

replaceAllRecordNums :: TimeLog -> String -> String -> TimeLog
replaceAllRecordNums timeLog old new = TimeLog (map fixNum
                                                (records timeLog))
                                       (current timeLog)
  where fixNum rcd = Record (if (recordNum rcd) == old
                              then new
                              else (recordNum rcd))
                     (inTime rcd)
                     (outTime rcd)
                     (description rcd)
                     (billable rcd)

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
  unless (null (records timeLog))
    $ putStrLn "Items worked on today:"
  _ <- sequence $ fmap putStrLn $ getRecordNums (records timeLog)
  num <- prompt "\nEnter item ID (Empty cancels):"
  case num of
    Just n -> do
      putStrLn $ "Clocked in at " ++ formatTime defaultTimeLocale "%R" time
      return $ TimeLog (records timeLog) (Just $ Record n time Nothing Nothing Nothing)
    Nothing -> do
      putStrLn canceledMessage
      return timeLog

clockOut :: TimeLog -> LocalTime -> IO TimeLog
clockOut timeLog time = do
  desc <- prompt "Enter a description of what you worked on (Empty cancels):"
  case desc of
    Just d -> do
      bill <- promptYN "Was this work billable?"
      let curr = fromJust $ current timeLog
          newRecord = Record (recordNum curr) (inTime curr) (Just time) (Just d) (Just bill)
      putStrLn $ "Clocked out at " ++ formatTime defaultTimeLocale "%R" time
      return $ TimeLog (records timeLog ++ [newRecord]) Nothing
    Nothing -> do
      putStrLn canceledMessage
      return timeLog

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
  hFlush stdout
  getLine >>= readYorN

seperatorLen :: Int
seperatorLen = 50

seperatorChar :: Char
seperatorChar = '-'

seperator :: String
seperator = replicate seperatorLen seperatorChar

printLog :: Day -> TimeLog -> IO (Maybe (TimeLog,Day))
printLog day timeLog = do
  putStrLn $ "\nTime log for " ++ formatTime defaultTimeLocale "%D" day
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
  putStrLn $ show mins ++ " Minute" ++ (if mins /= 1 then "s" else "") ++ ", " ++
                       if fromJust (billable rcd)
                       then "Billable"
                       else "Non-billable"

formatFileName :: Day -> String
formatFileName day = (formatTime defaultTimeLocale "%_Y%m%d" day)

saveTimeLog :: Day -> TimeLog -> IO ()
saveTimeLog day timeLog = do
  dataPath <- dataFilePath
  writeFile (dataPath ++ formatFileName day) $ show timeLog

loadTimeLog :: Day -> IO TimeLog
loadTimeLog day = do
  dataPath <- dataFilePath
  let fileName = dataPath ++ formatFileName day
  fileExists <- doesFileExist fileName
  if fileExists
    then do
      timeLog <- readFile fileName
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
