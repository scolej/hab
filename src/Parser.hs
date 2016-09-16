{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Parser (loadFile) where

import Data.Time
import Data.Maybe
import Text.Parsec hiding (spaces)
import Entry

loadFile :: String -> IO [Entry]
loadFile file = do
  contents <- readFile file
  case parse parseEntries file contents
    of Right es -> return es
       Left err -> error (show err)

burnTillEndOfLine :: Parsec String () ()
burnTillEndOfLine = skipMany $ noneOf "\n\r"

comment :: Parsec String () ()
comment = char '#' >> burnTillEndOfLine

day :: Parsec String () Day
day = do
  yyyy <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  dd <- count 2 digit
  return $ fromGregorian (read yyyy) (read mm) (read dd)

time :: Parsec String () TimeOfDay
time = do
  hh <- count 2 digit
  mm <- count 2 digit
  return $ TimeOfDay (read hh) (read mm) 0

entry :: Parsec String () Entry
entry = do
  d <- day
  space
  t <- time
  space
  action <- count 1 anyChar
  space
  e <- case action of "h" -> habit
                      "p" -> periodic
                      "t" -> todo
                      "x" -> mark
                      _ -> parserFail "Not a valid action." 
  return $ e (LocalTime d t)

parseLine :: Parsec String () (Maybe Entry)
parseLine = (comment >> return Nothing) <|> (Just <$> entry)

parseEntries :: Parsec String () [Entry]
parseEntries = catMaybes <$> parseLine `sepEndBy1` endOfLine

readWord :: Parsec String () String
readWord = many1 $ noneOf " "

spaces :: Parsec String () ()
spaces = skipMany1 space

parseDuration :: Parsec String () DiffTime
parseDuration = undefined

habit :: Parsec String () (LocalTime -> Entry)
habit = do
  name <- readWord
  spaces
  val <- readWord
  burnTillEndOfLine
  return $ EntryHabit (Habit name (read val))

periodic :: Parsec String () (LocalTime -> Entry)
periodic = do
  name <- readWord
  spaces
  d <- parseDuration
  spaces
  val <- readWord
  burnTillEndOfLine
  return $ EntryPeriodic (Periodic name d (read val))

todo :: Parsec String () (LocalTime -> Entry)
todo = undefined

mark :: Parsec String () (LocalTime -> Entry)
mark = undefined
