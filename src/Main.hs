import Text.Printf
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
import Data.Ord

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
    then do mapM_ print (gsMods g)
            print (charFromState g)
            writePeriodics g
    else do ms <- tryMarks names args
            let g' = followEntries g now (map EntryMark ms)
            postWrite g g'

  return ()

ppDiffTime :: DiffTime -> String
ppDiffTime d | abs d < 60 * 60 * 24 = printf "%.1fh" (d' / 60 / 60)
             | abs d < 60 * 60 * 24 * 7 = printf "%4.1fd" (d' / 60 / 60 / 24)
             | otherwise =  printf "%.1fw" (d' / 60 / 60 / 24 / 7)
  where d' = (fromRational . toRational) d :: Float

timeRemaining :: LocalTime -> GameStateAcc -> Periodic -> Maybe DiffTime
timeRemaining now g (Periodic p d _) =
  let u = localTimeToUTC utc
      lm = lookup p (gsLastPeriodicMark g)
  in case lm of Nothing -> Nothing
                Just l -> let dur = diffUTCTime (u now) (u l)
                          in Just $ d - (fromRational . toRational) dur

writePeriodics :: GameStateAcc -> IO ()
writePeriodics g = do
  now <- getLocalTime
  let ps = [p | ItemPeriodic p <- (map snd . gsItems) g]
      rs = map (\p -> (p, timeRemaining now g p)) ps
      rs' = [(p, r) | (p, Just r) <- rs]
  mapM_
    (\(Periodic n _ _, r) -> putStrLn $ unwords [ppDiffTime r, n])
    (sortBy (comparing snd) rs')

postWrite :: GameStateAcc -> GameStateAcc -> IO ()
postWrite gs gs' = do
  let c = charFromState gs
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
  let f = SetColor Foreground Vivid
  if i < 0
    then do setSGR [f Red]
            putStr $ replicate (-i) '-'
    else do setSGR [f Green]
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
