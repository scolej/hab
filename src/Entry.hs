module Entry ( Habit (..)
             , Periodic (..)
             , Todo (..)
             , Entry (..)
             , Nameable
             , getName
             ) where

import Data.Time

class Nameable s where
  getName :: s -> String

data Habit = Habit String Int
  deriving (Show)

instance Nameable Habit where
  getName (Habit s _) = s

data Periodic = Periodic String DiffTime Int
  deriving (Show)

instance Nameable Periodic where
  getName (Periodic s _ _) = s

data Todo = Todo String Int
  deriving (Show)

instance Nameable Todo where
  getName (Todo s _) = s

data Entry = EntryHabit    Habit    LocalTime
           | EntryPeriodic Periodic LocalTime
           | EntryTodo     Todo     LocalTime
           | EntryMark     String   LocalTime
  deriving (Show)
