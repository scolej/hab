import Control.Monad
import Game
import Parser
import System.Environment
import Data.Time
import Data.List
import Data.String.Utils

logFile = "/home/sjc/everything/habit.log"

getLocalTime = do
  tz <- getCurrentTimeZone
  now <- getCurrentTime
  return $ utcToLocalTime tz now

main :: IO ()
main = do
  args <- getArgs

  -- Read in a list of entries
  es <- loadFile logFile

  now <- getLocalTime

  let g = followEntries now es
      names = map fst (gsItems g)

  if (length args == 0)
    then mapM_ print $ gsMods g
    else tryMarks names args

  return ()

tryMarks :: [String] -> [String] -> IO ()
tryMarks names args = do
  let marks = map (disambigName names) args
  if all (\x -> case x of Match _ -> True; _ -> False) marks
    then writeMarks (map (\(Match x) -> x) marks)  >> putStrLn (show (length marks) ++ " item(s) checked off")
    else mapM_ print marks

data Match a = NoMatch a | Ambiguous [a] | Match a
  deriving Show

disambigName :: [String] -> String -> Match String
disambigName names arg = do
  let cands = filter (isInfixOf arg) names
  case cands of [a] -> Match a
                [] -> NoMatch arg
                xs -> Ambiguous xs

writeMarks :: [String] -> IO ()
writeMarks marks = do
  now <- getLocalTime
  let e m = unwords [formatTime defaultTimeLocale "%Y-%m-%d %H%M" now, "x", m]
  let newlines = "\n" ++ (concat . intersperse "\n" . map e) marks
  appendFile logFile newlines
