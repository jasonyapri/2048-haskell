import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT (runStateT), get, modify, put, state)
import Data.Data (Data, Typeable)
import System.IO

data Record = Record
  { name :: String,
    score :: Int
  }
  deriving (Show)

type Database = [Record]

initialDB :: Database
initialDB = []

createRecord :: Record -> StateT Database (MaybeT IO) ()
createRecord record = modify (\db -> record : db)

readRecord :: String -> StateT Database (MaybeT IO) Record
readRecord input = do
  db <- get
  let matchingRecords = filter (\record -> input == (name record)) db
  case matchingRecords of
    [] -> liftIO $ putStrLn "Record not found." >> fail "Record not found."
    (p : _) -> return p

updateRecord :: String -> Int -> StateT Database (MaybeT IO) ()
updateRecord input newAge = do
  record <- readRecord input
  modify (\db -> map (\p -> if (name p) == input then p {score = newAge} else p) db)

deleteRecord :: String -> StateT Database (MaybeT IO) ()
deleteRecord input = do
  record <- readRecord input
  modify (\db -> filter (\p -> (name p) /= input) db)

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

askForName :: StateT Database (MaybeT IO) ()
askForName = do
  name <- liftIO $ promptString "What's your name? "
  let record = Record name 0
  createRecord record
  liftIO $ putStrLn ("Welcome " ++ name ++ "!")
  liftIO $ putStrLn ""
  liftIO $ putStrLn ""

playGame :: StateT Database (MaybeT IO) ()
playGame = do
  liftIO $ putStrLn $ "10 minutes later..."
  liftIO $ putStrLn $ "You have played a game!"
  liftIO $ putStrLn ""
  mainMenu

showLeaderboard :: StateT Database (MaybeT IO) ()
showLeaderboard = do
  name <- liftIO $ promptString "Enter record's name: "
  record <- readRecord name
  liftIO $ putStrLn $ "record: " ++ show record
  liftIO $ putStrLn ""
  mainMenu

modifyLeaderboard :: StateT Database (MaybeT IO) ()
modifyLeaderboard = do
  name <- liftIO $ promptString "Enter a name: "
  score <- liftIO $ promptInt "Enter the score: "
  updateRecord name score
  liftIO $ putStrLn "Score updated."
  liftIO $ putStrLn ""
  mainMenu

removeRecordFormLeaderboard :: StateT Database (MaybeT IO) ()
removeRecordFormLeaderboard = do
  name <- liftIO $ promptString "Enter record's name: "
  deleteRecord name
  liftIO $ putStrLn "Score Deleted."
  liftIO $ putStrLn ""
  mainMenu

resetLeaderboard :: StateT Database (MaybeT IO) ()
resetLeaderboard = do
  -- Reset Leaderboard Logic
  liftIO $ putStrLn "Leaderboard has been Reset"
  liftIO $ putStrLn ""
  mainMenu

mainMenu :: StateT Database (MaybeT IO) ()
mainMenu = do
  liftIO $ putStrLn "==== Main Menu ===="
  liftIO $ putStrLn "1. Play"
  liftIO $ putStrLn "2. Show Leaderboard"
  liftIO $ putStrLn "3. Hack Leaderboard (Modify)"
  liftIO $ putStrLn "4. Hack Leaderboard (Remove)"
  liftIO $ putStrLn "5. Hack Leaderboard (Reset)"
  liftIO $ putStrLn "0. Exit"
  choice <- liftIO $ promptInt "Enter your choice: "
  liftIO $ putStrLn ""
  case choice of
    1 -> playGame
    2 -> showLeaderboard
    3 -> modifyLeaderboard
    4 -> removeRecordFormLeaderboard
    5 -> resetLeaderboard
    0 -> liftIO $ putStrLn "Thanks for playing!"

main :: IO ()
main = do
  putStrLn "***********************************"
  putStrLn "****** 2048 GAME IN HASKELL *******"
  putStrLn "***** created by Jason Yapri ******"
  putStrLn "***********************************"

  putStrLn ""
  let combinedActions = askForName >> mainMenu

  result <- runMaybeT (runStateT combinedActions [])
  case result of
    Just (_, finalDatabase) -> do
      putStrLn "~Come back anytime~"
    Nothing ->
      putStrLn "~Come back anytime~"
