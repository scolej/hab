import Debug.Trace
import Control.Monad
import Game
import Parser
import System.Environment
import Data.Time
import Data.List
import Data.String.Utils
import Entry
import System.Console.ANSI

logFile :: String
logFile = "/home/sjc/everything/habit.log"

getLocalTime :: IO LocalTime
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

  let g = followEntries blankState now es
      names = map fst (gsItems g)

  if null args
    then mapM_ print (gsMods g) >> print (charFromState g)
    else tryMarks names args >>= postWrite g

  return ()

postWrite :: GameStateAcc -> [Mark] -> IO ()
postWrite gs es = do
  now <- getLocalTime
  let c = charFromState gs
      gs' = followEntries gs now (map EntryMark es)
      c' = charFromState gs'
      dh = csLife c' - csLife c
      dx = csExp c' - csExp c

  when (dx /= 0) $ do
    putStr "Exp: "
    colourMarks dx
    putStr "\n"

  when (dh /= 0) $ do
    putStr "Lif: "
    colourMarks dh
    putStr "\n"

colourMarks :: Int -> IO ()
colourMarks i = do
  if i < 0
    then do setSGR [SetColor Background Vivid Red]
            putStr $ replicate (-i) '-'
    else do setSGR [SetColor Background Vivid Green]
            putStr $ replicate i '+'
  setSGR []

tryMarks :: [String] -> [String] -> IO [Mark]
tryMarks names args = do
  now <- getLocalTime
  let marks = map (disambigName names) args
  if all (\x -> case x of Match _ -> True; _ -> False) marks
    then do let ms = map (\(Match x) -> x) marks
                es = map (\s -> (s, now)) ms
            writeMarks es
            putStrLn (show (length marks) ++ " item(s) checked off")
            return es
    else mapM_ print marks >> return []

data Match a = NoMatch a | Ambiguous [a] | Match a
  deriving Show

disambigName :: [String] -> String -> Match String
disambigName names arg = do
  let cands = filter (isInfixOf arg) names
  case cands of [a] -> Match a
                [] -> NoMatch arg
                xs -> Ambiguous xs

markStr :: Mark -> String
markStr (s, t) = unwords [formatTime defaultTimeLocale "%Y-%m-%d %H%M" t, "x", s]

writeMarks :: [Mark] -> IO ()
writeMarks marks = do
  let newlines = "\n" ++ (intercalate "\n" . map markStr) marks
  appendFile logFile newlines
