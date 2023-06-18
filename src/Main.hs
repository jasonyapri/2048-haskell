-- \****** 2048 GAME IN HASKELL *******
-- \***** created by Jason Yapri ******
-- \**** https://jasonyapri.com/ ******

import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT (runStateT), execStateT, get, modify, put, state)
import Data.ByteString (find)
import Data.Data (Data, Typeable)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Time
import Data.Time.Format
import System.IO
import System.Random
import Text.Printf

type Log = [String]

data Record = Record
  { name :: String,
    score :: Int
  }
  deriving (Show)

type Name = String

type Score = String

data Database = Database
  { records :: [Record],
    currentPlayerName :: String,
    currentScore :: Int,
    board :: [[Int]],
    gameOver :: Bool,
    didntMove :: Bool
  }
  deriving (Show)

initialDB :: Database
initialDB =
  Database
    { records = [],
      currentPlayerName = "",
      currentScore = 0,
      board = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]],
      gameOver = False,
      didntMove = False
    }

data Move = MoveUp | MoveDown | MoveLeft | MoveRight | MoveExit
  deriving (Show)

recordLog :: String -> StateT Log IO ()
recordLog entry = do
  currentTime <- liftIO getCurrentTime -- Get the current time in IO monad
  let timestamp = formatTime defaultTimeLocale "%A, %B %d, %Y | %H:%M:%S" currentTime
  modify (\log -> ("[INFO] - " ++ timestamp ++ " (GMT) " ++ " - " ++ entry) : log)

saveLogsToFile :: FilePath -> StateT Log IO ()
saveLogsToFile filepath = do
  logs <- get
  liftIO $ withFile filepath AppendMode $ \h -> do
    mapM_ (hPutStrLn h) (reverse logs)

createRecord :: Record -> StateT Database (MaybeT IO) ()
createRecord record = modify (\db -> db {records = record : records db})

readHighScore :: String -> StateT Database (MaybeT IO) (Maybe Int)
readHighScore input = do
  db <- get
  let matchingRecords = filter (\record -> input == name record) (records db)
  case matchingRecords of
    [] -> return Nothing
    (p : _) -> return (Just (score p))

readRecord :: String -> StateT Database (MaybeT IO) (Maybe Record)
readRecord input = do
  db <- get
  let matchingRecords = filter (\record -> input == (name record)) (records db)
  case matchingRecords of
    [] -> do
      return Nothing
    (p : _) -> return (Just p)

updateRecord :: String -> Int -> Bool -> StateT Database (MaybeT IO) ()
updateRecord input newScore isVerbose = do
  record <- readRecord input
  case record of
    Nothing -> when isVerbose $ liftIO $ putStrLn "Name not found."
    Just rec -> do
      when isVerbose $ liftIO $ putStrLn "Score updated."
      modify (\db -> db {records = map updateScore (records db)})
      where
        updateScore p
          | name p == input = p {score = newScore}
          | otherwise = p

deleteRecord :: String -> StateT Database (MaybeT IO) ()
deleteRecord input = do
  record <- readRecord input
  case record of
    Nothing -> liftIO $ putStrLn "Name not found."
    Just rec -> do
      liftIO $ putStrLn ("Deleted " ++ input)
      modify (\db -> db {records = filter (\p -> name p /= input) (records db)})

promptString :: String -> IO String
promptString prompt = do
  putStr prompt
  hFlush stdout
  getLine

promptInt :: String -> IO Int
promptInt prompt = do
  str <- promptString prompt
  case reads str of
    [(x, "")] -> return x
    _ -> do
      putStrLn "Invalid input. Please enter an integer."
      promptInt prompt

setCurrentPlayerName :: String -> StateT Database (MaybeT IO) ()
setCurrentPlayerName newName = modify (\db -> db {currentPlayerName = newName})

checkNameExists :: String -> StateT Database (MaybeT IO) Bool
checkNameExists nameToCheck = do
  db <- get
  let exists = any (\r -> name r == nameToCheck) (records db)
  return exists

askForName :: StateT Database (MaybeT IO) ()
askForName = do
  name <- liftIO $ promptString "What's your name? "
  let record = Record name 0
  nameExists <- checkNameExists name
  unless nameExists $ createRecord record

  liftIO $ putStrLn ("Welcome " ++ name ++ "!")
  liftIO $ putStrLn ""
  liftIO $ putStrLn ""
  setCurrentPlayerName name

placeRandomTile :: Database -> StateT Database (MaybeT IO) (Database, Bool)
placeRandomTile db = do
  -- Retrieve the current state of the Database
  let boardSize = length (board db)
      emptyCells = [(row, col) | row <- [0 .. boardSize - 1], col <- [0 .. boardSize - 1], (board db !! row) !! col == 0]

  -- Check if there are empty cells on the board
  if null emptyCells
    then do
      -- No empty cells, game over
      return (db, True)
    else do
      -- Choose a random empty cell
      gen <- liftIO newStdGen
      let randomIndex = fst $ randomR (0, length emptyCells - 1) gen
          (row, col) = emptyCells !! randomIndex

      let tile = 2

      -- Place the tile on the board
      let updatedBoard = update2DList (board db) row col tile

      -- Update the state with the new board
      let updatedDB = db {board = updatedBoard}
      return (updatedDB, False)

-- Helper function to update a value in a 2D list
update2DList :: [[a]] -> Int -> Int -> a -> [[a]]
update2DList list row col val =
  take row list
    ++ [take col (list !! row) ++ [val] ++ drop (col + 1) (list !! row)]
    ++ drop (row + 1) list

printBoard :: [[Int]] -> IO ()
printBoard = mapM_ (putStrLn . unwords . map show)

generateRandomNumber :: IO Int
generateRandomNumber = randomRIO (1, 2048)

newGame :: StateT Database (MaybeT IO) ()
newGame = do
  liftIO $ do
    (_, _) <- runStateT printNewGameLog []
    return ()
  modify (\db -> db {currentScore = 0, board = [[0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0], [0, 0, 0, 0]], didntMove = False})

getPlayerMove :: IO Move
getPlayerMove = do
  putStrLn "Enter your move (wasd) or quit (q):"
  moveStr <- getLine
  case moveStr of
    "w" -> return MoveUp
    "a" -> return MoveLeft
    "s" -> return MoveDown
    "d" -> return MoveRight
    "q" -> return MoveExit
    _ -> do
      putStrLn "Invalid move!\n"
      getPlayerMove

moveBoard :: Move -> Database -> Database
moveBoard move db = case move of
  MoveUp -> swipeUp db
  MoveDown -> swipeDown db
  MoveLeft -> swipeLeft db
  MoveRight -> swipeRight db
  MoveExit -> setBoardValues db

-- Move functions be defined
swipeUp :: Database -> Database
swipeUp db =
  let originalArray = board db
      transposedArray = transpose originalArray
      (newArray, didntMove) = foldr moveColumn ([], True) transposedArray
      updatedArray = transpose newArray
   in if not didntMove && updatedArray /= originalArray
        then db {board = updatedArray, didntMove = False}
        else db {didntMove = True}
  where
    moveColumn :: [Int] -> ([[Int]], Bool) -> ([[Int]], Bool)
    moveColumn column (acc, didntMove) =
      let nonZeroValues = [value | value <- column, value /= 0]
          mergedColumn = mergeAdjacent nonZeroValues
          newColumn = padZeros mergedColumn (length column)
          updatedAcc = newColumn : acc
       in (updatedAcc, didntMove && (column == newColumn))

    mergeAdjacent :: [Int] -> [Int]
    mergeAdjacent [] = []
    mergeAdjacent [x] = [x]
    mergeAdjacent (x : y : xs)
      | x == y = x * 2 : mergeAdjacent xs
      | otherwise = x : mergeAdjacent (y : xs)

    padZeros :: [Int] -> Int -> [Int]
    padZeros lst len = lst ++ replicate (len - length lst) 0

swipeDown :: Database -> Database
swipeDown db =
  let originalArray = board db
      reversedArray = reverse originalArray
      transposedArray = transpose reversedArray
      (newArray, didntMove) = foldr moveColumn ([], True) transposedArray
      updatedArray = reverse (transpose newArray)
   in if not didntMove && updatedArray /= originalArray
        then db {board = updatedArray, didntMove = False}
        else db {didntMove = True}
  where
    moveColumn :: [Int] -> ([[Int]], Bool) -> ([[Int]], Bool)
    moveColumn column (acc, didntMove) =
      let nonZeroValues = [value | value <- column, value /= 0]
          mergedColumn = mergeAdjacent nonZeroValues
          newColumn = padZeros mergedColumn (length column)
          updatedAcc = newColumn : acc
       in (updatedAcc, didntMove && (column == newColumn))

    mergeAdjacent :: [Int] -> [Int]
    mergeAdjacent [] = []
    mergeAdjacent [x] = [x]
    mergeAdjacent (x : y : xs)
      | x == y = x * 2 : mergeAdjacent xs
      | otherwise = x : mergeAdjacent (y : xs)

    padZeros :: [Int] -> Int -> [Int]
    padZeros lst len = lst ++ replicate (len - length lst) 0

swipeLeft :: Database -> Database
swipeLeft db =
  let originalArray = board db
      (newArray, didntMove) = foldr moveRow ([], True) originalArray
   in if not didntMove && newArray /= originalArray
        then db {board = newArray, didntMove = False}
        else db {didntMove = True}
  where
    moveRow :: [Int] -> ([[Int]], Bool) -> ([[Int]], Bool)
    moveRow row (acc, didntMove) =
      let nonZeroValues = [value | value <- row, value /= 0]
          mergedRow = mergeAdjacent nonZeroValues
          newRow = padZeros mergedRow (length row)
          updatedAcc = newRow : acc
       in (updatedAcc, didntMove && (row == newRow))

    mergeAdjacent :: [Int] -> [Int]
    mergeAdjacent [] = []
    mergeAdjacent [x] = [x]
    mergeAdjacent (x : y : xs)
      | x == y = x * 2 : mergeAdjacent xs
      | otherwise = x : mergeAdjacent (y : xs)

    padZeros :: [Int] -> Int -> [Int]
    padZeros lst len = lst ++ replicate (len - length lst) 0

swipeRight :: Database -> Database
swipeRight db =
  let originalArray = board db
      (newArray, didntMove) = foldr moveRow ([], True) originalArray
      updatedArray = newArray
   in if not didntMove && updatedArray /= originalArray
        then db {board = updatedArray, didntMove = False}
        else db {didntMove = True}
  where
    moveRow :: [Int] -> ([[Int]], Bool) -> ([[Int]], Bool)
    moveRow row (acc, didntMove) =
      let nonZeroValues = [value | value <- row, value /= 0]
          mergedRow = mergeAdjacent nonZeroValues
          newRow = padZeros mergedRow (length row)
          updatedAcc = newRow : acc
       in (updatedAcc, didntMove && (row == newRow))

    mergeAdjacent :: [Int] -> [Int]
    mergeAdjacent [] = []
    mergeAdjacent [x] = [x]
    mergeAdjacent (x : y : xs)
      | x == y = x * 2 : mergeAdjacent xs
      | otherwise = x : mergeAdjacent (y : xs)

    padZeros :: [Int] -> Int -> [Int]
    padZeros lst len = replicate (len - length lst) 0 ++ lst

setBoardValues :: Database -> Database
setBoardValues db = db {board = replicate 4 (replicate 4 9)}

placeOrKeepTile :: Database -> StateT Database (MaybeT IO) (Database, Bool)
placeOrKeepTile db = do
  -- liftIO $ putStrLn ("LOG: didntMove value: " ++ (show (didntMove db))) -- NOTE: Comment in production
  if not (didntMove db)
    then placeRandomTile db
    else return (db {didntMove = False}, False)

playGame :: Int -> StateT Database (MaybeT IO) ()
playGame score = do
  db <- get
  -- Get Score and print it
  liftIO $ putStrLn ("Your Score: " ++ show score)

  -- Generate a random tile 2 and place it on the board
  let isGameOver = gameOver db
  (dbWithTile, isGameOver) <- placeOrKeepTile db

  -- Print the updated board
  liftIO $ putStrLn "Game Board:"
  liftIO $ printBoard (board dbWithTile)

  -- Check if the game is over
  if isGameOver
    then do
      liftIO $ putStrLn "Game over!"
      liftIO $ putStrLn ""
      -- Update the score only if there is a new high score
      highScore <- readHighScore (currentPlayerName db)
      if score > fromMaybe 0 highScore
        then do
          updateRecord (currentPlayerName db) score False
          liftIO $ putStrLn ("Your last score: " ++ show score ++ "(NEW HIGH SCORE)")
          liftIO $ putStrLn ""
        else do
          liftIO $ putStrLn ("Current high score: " ++ show (fromMaybe 0 highScore))
      mainMenu
    else do
      -- Read the player's move
      move <- liftIO getPlayerMove

      -- Perform the move and update the game state
      let newDB = moveBoard move dbWithTile
      liftIO $ putStrLn ""
      -- Update the state with the new database
      put newDB

      -- Continue playing the game
      let newScore = sum (concat (board dbWithTile))
      playGame newScore

showRecord :: Record -> String
showRecord r = printf "%-20s %10d" (name r) (score r)

showLeaderboard :: StateT Database (MaybeT IO) ()
showLeaderboard = do
  liftIO $ do
    (_, _) <- runStateT printShowLeaderboardLog []
    return ()
  db <- get
  let header = printf "%-20s %10s\n" "Name" "Score"
  let recordsList = map showRecord (records db)
  let table = header ++ unlines recordsList
  liftIO $ putStrLn table
  mainMenu

switchPlayer :: StateT Database (MaybeT IO) ()
switchPlayer = do
  liftIO $ do
    (_, _) <- runStateT printSwitchPlayerLog []
    return ()
  askForName
  mainMenu

modifyLeaderboard :: StateT Database (MaybeT IO) ()
modifyLeaderboard = do
  liftIO $ do
    (_, _) <- runStateT printModifyLeaderboardLog []
    return ()
  name <- liftIO $ promptString "Enter a name: "
  score <- liftIO $ promptInt "Enter the score: "
  updateRecord name score True
  liftIO $ putStrLn ""
  mainMenu

removeRecordFormLeaderboard :: StateT Database (MaybeT IO) ()
removeRecordFormLeaderboard = do
  liftIO $ do
    (_, _) <- runStateT printRemoveLeaderboardLog []
    return ()
  name <- liftIO $ promptString "Enter record's name: "
  db <- get
  if name == currentPlayerName db
    then do
      liftIO $ putStrLn "Cannot delete current player. Please switch player first."
    else do
      deleteRecord name
  liftIO $ putStrLn ""
  mainMenu

resetLeaderboard :: StateT Database (MaybeT IO) ()
resetLeaderboard = do
  liftIO $ do
    (_, _) <- runStateT printResetLeaderboardLog []
    return ()
  modify (\db -> db {records = []})
  db <- get
  let record = Record (currentPlayerName db) 0
  nameExists <- checkNameExists (currentPlayerName db)
  unless nameExists $ createRecord record
  liftIO $ putStrLn "Leaderboard has been Reset"
  liftIO $ putStrLn ""
  mainMenu

recordToString :: Record -> String
recordToString (Record name score) = name ++ "\t" ++ show score

databaseToString :: Database -> String
databaseToString (Database records _ _ _ _ _) = unlines $ map recordToString records

mainMenu :: StateT Database (MaybeT IO) ()
mainMenu = do
  liftIO $ putStrLn "********** Main Menu **********"
  liftIO $ putStrLn "1. Play"
  liftIO $ putStrLn "2. Show Leaderboard"
  liftIO $ putStrLn "3. Switch Player"
  liftIO $ putStrLn "4. Hack Leaderboard (Modify)"
  liftIO $ putStrLn "5. Hack Leaderboard (Remove)"
  liftIO $ putStrLn "6. Hack Leaderboard (Reset)"
  liftIO $ putStrLn "9. Show How To Play (?)"
  liftIO $ putStrLn "0. Exit"
  choice <- liftIO $ promptInt "Enter your choice: "
  liftIO $ putStrLn ""
  case choice of
    1 -> do
      db <- get
      newGame
      playGame 0
    2 -> showLeaderboard
    3 -> switchPlayer
    4 -> modifyLeaderboard
    5 -> removeRecordFormLeaderboard
    6 -> resetLeaderboard
    9 -> do
      liftIO $ do
        (_, _) <- runStateT printShowGameInstructionLog []
        return ()
      liftIO $ putStrLn "========= HOW TO PLAY ========="
      liftIO $ putStrLn "1. Use wasd to move the tiles."
      liftIO $ putStrLn "2. Tiles with the same number merge into one when they touch. "
      liftIO $ putStrLn "3. Add them up to reach 2048!"
      liftIO $ putStrLn ""
      liftIO $ putStrLn "Good Luck and Have Fun :D"
      liftIO $ putStrLn "==============================="
      liftIO $ putStrLn ""
      mainMenu
    0 -> do
      liftIO $ do
        (_, _) <- runStateT printExitGameLog []
        return ()
      db <- get
      liftIO $ putStrLn ("Thanks for playing, " ++ currentPlayerName db)
      -- let outputContents = "A\t1024\nB\t512"
      let outputContents = databaseToString db
      liftIO $ writeFile "leaderboard.txt" outputContents
      liftIO $ putStrLn ""
      liftIO $ putStrLn "Saving leaderboard to database..."
      liftIO $ putStrLn ""
      liftIO $ putStrLn ""

printNewGameLog :: StateT Log IO ()
printNewGameLog = do
  recordLog "Initiating a new game"
  saveLogsToFile "logs.txt"

printShowLeaderboardLog :: StateT Log IO ()
printShowLeaderboardLog = do
  recordLog "Showing Leaderboard"
  saveLogsToFile "logs.txt"

printSwitchPlayerLog :: StateT Log IO ()
printSwitchPlayerLog = do
  recordLog "Switching Player"
  saveLogsToFile "logs.txt"

printModifyLeaderboardLog :: StateT Log IO ()
printModifyLeaderboardLog = do
  recordLog "Modifying Leaderboard"
  saveLogsToFile "logs.txt"

printRemoveLeaderboardLog :: StateT Log IO ()
printRemoveLeaderboardLog = do
  recordLog "Removing Leaderboard"
  saveLogsToFile "logs.txt"

printResetLeaderboardLog :: StateT Log IO ()
printResetLeaderboardLog = do
  recordLog "Reseting Leaderboard"
  saveLogsToFile "logs.txt"

printShowGameInstructionLog :: StateT Log IO ()
printShowGameInstructionLog = do
  recordLog "Showing Game Instruction"
  saveLogsToFile "logs.txt"

printExitGameLog :: StateT Log IO ()
printExitGameLog = do
  recordLog "Exiting game"
  saveLogsToFile "logs.txt"

parseLine :: String -> (Name, Score)
parseLine line =
  case splitOn "\t" line of
    [name, score] -> (name, score)
    _ -> ("", "")

processNameAndScore :: (Name, Score) -> StateT Database (MaybeT IO) ()
processNameAndScore (name, score) = do
  let record = Record name (read score :: Int)
  nameExists <- checkNameExists name
  unless nameExists $ createRecord record

main :: IO ()
main = do
  putStrLn "***********************************"
  putStrLn "****** 2048 GAME IN HASKELL *******"
  putStrLn "***** created by Jason Yapri ******"
  putStrLn "**** https://jasonyapri.com/ ******"
  putStrLn "***********************************"
  putStrLn ""

  putStrLn "Loading leaderboard from database..."
  handle <- openFile "leaderboard.txt" ReadMode
  contents <- liftIO $ hGetContents handle
  liftIO $ putStr contents
  liftIO $ hClose handle
  putStrLn ""
  putStrLn ""

  let linesOfContent = lines contents
  let nameAndScores = map parseLine linesOfContent

  let combinedActions = mapM_ processNameAndScore nameAndScores >> askForName >> mainMenu

  result <- runMaybeT (runStateT combinedActions initialDB)

  case result of
    Just (_, finalDatabase) -> do
      putStrLn "Come back anytime :D"
    Nothing ->
      putStrLn "Come back anytime :D"

-- ****** 2048 GAME IN HASKELL *******

-- ***** created by Jason Yapri ******

-- **** https://jasonyapri.com/ ******