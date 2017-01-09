import Control.Monad
import Data.List
import Data.Ord
import Data.Time
import Entry
import Game
import Parser
import System.Console.ANSI
import System.Directory
import System.Environment
import System.FilePath
import Text.Printf

-- | Name of the file in the user's home directory where the log can
-- be read from.
logFile :: String
logFile = "habit.log"

getLocalTime :: IO LocalTime
getLocalTime = do
  tz <- getCurrentTimeZone
  now <- getCurrentTime
  return $ utcToLocalTime tz now

main :: IO ()
main = do
  args <- getArgs
  home <- getHomeDirectory
  -- Read in a list of entries.
  es <- loadFile (home </> logFile)
  now <- getLocalTime
  -- Run all the entries from a blank state.
  let (g, c) = runEntries (blankState, blankCharacter) es now
  if null args
    then doStatusReport g c
    else doMarks now args (g, c)
  return ()

-- | Branch of main in which the program should attempt to mark off items and emit a report of the resultant marks.
doMarks
  :: LocalTime -- ^ The current time.
  -> [String] -- ^ List of strings which were passed as arguments.
  -> (GameState, CharState) -- ^ Game and character state before the marks are applied.
  -> IO ()
doMarks now args (g, c) = do
  let names = map fst (gsItems g)
  ms <- tryMarks names args
  let (_, c') = runEntries (g, c) (map EntryMark ms) now
  unless (null ms) $ postWrite c c'

-- | Branch of main in which we simply emit a report of the current situation.
doStatusReport
  :: GameState -- ^ Current game state.
  -> CharState -- ^ Current character state.
  -> IO ()
doStatusReport g c = do
  mapM_ ppCharMod (reverse $ gsMods g)
  (putStrLn . charSummary) c
  printBars c
  writePeriodics g

-- | Width of column for printing the name of things checked off.
nameWidth :: Int
nameWidth = 40

-- | Print time without seconds.
showDateToMin :: LocalTime -> String
showDateToMin = formatTime defaultTimeLocale "%Y-%m-%d %H:%M"

-- | Pretty print a character modifications with colours and crosses and minuses.
ppCharMod :: CharMod -> IO ()
ppCharMod (ModHealth d n x) = ppMod d n x Red
ppCharMod (ModExp d n x) = ppMod d n x Green

ppMod :: LocalTime -> String -> Int -> Color -> IO ()
ppMod time name x color = do
  putStr $ unwords [showDateToMin time, spacePad nameWidth name]
  putStr " "
  setSGR [SetColor Foreground Vivid color]
  putStr $
    replicate
      (abs x)
      (if x < 0
         then '-'
         else '+')
  setSGR [Reset]
  putStr "\n"

ppDiffTime :: DiffTime -> String
ppDiffTime d
  | abs d < 60 * 60 * 24 = printf "%8.1fh" (d' / 60 / 60)
  | abs d < 60 * 60 * 24 * 7 = printf "%8.1fd" (d' / 60 / 60 / 24)
  | otherwise = printf "%8.1fw" (d' / 60 / 60 / 24 / 7)
  where
    d' = (fromRational . toRational) d :: Float

timeRemaining :: LocalTime -> GameState -> Periodic -> Maybe DiffTime
timeRemaining now g (Periodic p d _) =
  let u = localTimeToUTC utc
      lm = lookup p (gsMarks g)
  in case lm of
       Nothing -> Nothing
       Just l ->
         let dur = diffUTCTime (u now) (u l)
         in Just $ d - (fromRational . toRational) dur

writePeriodics :: GameState -> IO ()
writePeriodics g = do
  now <- getLocalTime
  let ps =
        [ p
        | ItemPeriodic p <- (map snd . gsItems) g ]
      rs = map (\p -> (p, timeRemaining now g p)) ps
      rs' =
        [ (p, r)
        | (p, Just r) <- rs ]
  mapM_
    (\(Periodic n _ _, r) -> putStrLn $ unwords [ppDiffTime r, n])
    (sortBy (comparing (\(Periodic _ d _, r) -> r / d)) rs')

postWrite :: CharState -> CharState -> IO ()
postWrite (CharState h0 e0 _) (CharState h1 e1 _) = do
  let dh = h1 - h0
      dx = e1 - e0
  when (dx /= 0) $
    do putStr "Exp: "
       colourMarks dx
       putStr "\n"
  when (dh /= 0) $
    do putStr "Lif: "
       colourMarks dh
       putStr "\n"

colourMarks :: Int -> IO ()
colourMarks i = do
  let f = SetColor Foreground Vivid
  if i < 0
    then do
      setSGR [f Red]
      putStr $ replicate (-i) '-'
    else do
      setSGR [f Green]
      putStr $ replicate i '+'
  setSGR [Reset]

tryMarks :: [String] -> [String] -> IO [Mark]
tryMarks names args = do
  now <- getLocalTime
  let marks = map (disambigName names) args
  if all
       (\x ->
           case x of
             Match _ -> True
             _ -> False)
       marks
    then do
      let ms = map (\(Match x) -> (x, now)) marks
      writeMarks ms
      putStrLn (show (length marks) ++ " item(s) checked off")
      return ms
    else mapM_ print marks >> return []

data Match a
  = NoMatch a
  | Ambiguous [a]
  | Match a
  deriving (Show)

disambigName :: [String] -> String -> Match String
disambigName names arg = do
  let cands = filter (isInfixOf arg) names
  case cands of
    [a] -> Match a
    [] -> NoMatch arg
    xs -> Ambiguous xs

markStr :: Mark -> String
markStr (s, t) =
  unwords [formatTime defaultTimeLocale "%Y-%m-%d %H%M" t, "x", s]

writeMarks :: [Mark] -> IO ()
writeMarks marks = do
  let newlines = "\n" ++ (intercalate "\n" . map markStr) marks
  appendFile logFile newlines

printBar :: Float -> IO ()
printBar x = do
  let w = 40
      a = floor $ x * fi w
      b = w - a
  putStr "["
  putStr $ replicate a 'x'
  putStr $ replicate b '-'
  putStr "]\n"

fi = fromIntegral

spacePad :: Int -> String -> String
spacePad i s = s ++ replicate (i - length s) ' '

printBars :: CharState -> IO ()
printBars (CharState h x l) = do
  let xm = lvlExp l
      hm = fullHealth
  -- Print experience bar.
  putStr $ spacePad 15 "Experience:"
  printBar (fi x / fi xm)
  -- Print health bar.
  putStr $ spacePad 15 "Health:"
  printBar (fi h / fi hm)
