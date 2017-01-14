{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module Hab.Parser
  ( loadFile
  ) where

import Data.Maybe
import Data.Time
import Hab.Entry
import Text.Parsec hiding (spaces)

loadFile :: String -> IO [Entry]
loadFile file = do
  contents <- readFile file
  case parse parseEntries file contents of
    Right es -> return es
    Left err -> error (show err)

parseLine :: Parsec String () (Maybe Entry)
parseLine = (comment >> return Nothing) <|> (Just <$> entry)

parseEntries :: Parsec String () [Entry]
parseEntries = catMaybes <$> parseLine `sepEndBy1` many1 endOfLine

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
  action <- anyChar
  space
  case action of
    'h' -> habit (LocalTime d t)
    'p' -> periodic (LocalTime d t)
    'x' -> mark (LocalTime d t)
    _ -> parserFail "not a valid action"

readWord :: Parsec String () String
readWord = many1 $ noneOf " \n\r"

spaces :: Parsec String () ()
spaces = skipMany1 space

parseDuration :: Parsec String () DiffTime
parseDuration = do
  xs <- many digit
  u <- anyChar
  let x = read xs
  s <-
    case u of
      'h' -> return $ 60 * 60
      'd' -> return $ 60 * 60 * 24
      'w' -> return $ 60 * 60 * 24 * 7
      _ -> parserFail "unknown duration unit"
  return $ secondsToDiffTime (x * s)

habit :: LocalTime -> Parsec String () Entry
habit t = do
  name <- readWord
  spaces
  val <- readWord
  burnTillEndOfLine
  return $ EntryHabit t (Habit name (read val))

periodic :: LocalTime -> Parsec String () Entry
periodic t = do
  name <- readWord
  spaces
  d <- parseDuration
  spaces
  val <- readWord
  burnTillEndOfLine
  return $ EntryPeriodic t (Periodic name d (read val))

mark :: LocalTime -> Parsec String () Entry
mark t = do
  name <- readWord
  burnTillEndOfLine
  return $ EntryMark (name, t)
