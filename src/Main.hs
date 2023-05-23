import Control.Monad
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT (runStateT), get, modify, put, state)
import Data.ByteString (find)
import Data.Data (Data, Typeable)
import System.IO
import Text.Printf

data Record = Record
  { name :: String,
    score :: Int
  }
  deriving (Show)

data Database = Database
  { records :: [Record],
    currentPlayerName :: String,
    currentPlayerHighScore :: Int
  }
  deriving (Show)

initialDB :: Database
initialDB =
  Database
    { records = [],
      currentPlayerName = "",
      currentPlayerHighScore = 0
    }

createRecord :: Record -> StateT Database (MaybeT IO) ()
createRecord record = modify (\db -> db {records = record : records db})

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

playGame :: StateT Database (MaybeT IO) ()
playGame = do
  liftIO $ putStrLn "10 minutes later..."
  liftIO $ putStrLn "You have played a game!"
  liftIO $ putStrLn ""
  mainMenu

showRecord :: Record -> String
showRecord r = printf "%-20s %10d" (name r) (score r)

showLeaderboard :: StateT Database (MaybeT IO) ()
showLeaderboard = do
  db <- get
  let header = printf "%-20s %10s\n" "Name" "Score"
  let recordsList = map showRecord (records db)
  let table = header ++ unlines recordsList
  liftIO $ putStrLn table
  mainMenu

switchPlayer :: StateT Database (MaybeT IO) ()
switchPlayer = do
  askForName
  mainMenu

modifyLeaderboard :: StateT Database (MaybeT IO) ()
modifyLeaderboard = do
  name <- liftIO $ promptString "Enter a name: "
  score <- liftIO $ promptInt "Enter the score: "
  updateRecord name score True
  liftIO $ putStrLn ""
  mainMenu

removeRecordFormLeaderboard :: StateT Database (MaybeT IO) ()
removeRecordFormLeaderboard = do
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
  modify (\db -> db {records = []})
  db <- get
  let record = Record (currentPlayerName db) 0
  nameExists <- checkNameExists (currentPlayerName db)
  unless nameExists $ createRecord record
  liftIO $ putStrLn "Leaderboard has been Reset"
  liftIO $ putStrLn ""
  mainMenu

mainMenu :: StateT Database (MaybeT IO) ()
mainMenu = do
  liftIO $ putStrLn "********** Main Menu **********"
  liftIO $ putStrLn "1. Play"
  liftIO $ putStrLn "2. Show Leaderboard"
  liftIO $ putStrLn "3. Switch Player"
  liftIO $ putStrLn "4. Hack Leaderboard (Modify)"
  liftIO $ putStrLn "5. Hack Leaderboard (Remove)"
  liftIO $ putStrLn "6. Hack Leaderboard (Reset)"
  liftIO $ putStrLn "0. Exit"
  choice <- liftIO $ promptInt "Enter your choice: "
  liftIO $ putStrLn ""
  case choice of
    1 -> playGame
    2 -> showLeaderboard
    3 -> switchPlayer
    4 -> modifyLeaderboard
    5 -> removeRecordFormLeaderboard
    6 -> resetLeaderboard
    0 -> do
      db <- get
      liftIO $ putStrLn ("Thanks for playing, " ++ currentPlayerName db)

main :: IO ()
main = do
  putStrLn "***********************************"
  putStrLn "****** 2048 GAME IN HASKELL *******"
  putStrLn "***** created by Jason Yapri ******"
  putStrLn "***********************************"

  putStrLn ""
  let combinedActions = askForName >> mainMenu

  result <- runMaybeT (runStateT combinedActions initialDB)
  case result of
    Just (_, finalDatabase) -> do
      putStrLn "Come back anytime :D"
    Nothing ->
      putStrLn "Come back anytime :D"
