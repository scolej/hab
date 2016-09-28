module Entry ( Entry (..)
             , Habit (..)
             , Mark
             , Nameable
             , Periodic (..)
             , diffLocalTime
             , addLocalTime
             , entryTime
             , getName
             ) where

import Data.Time

data Periodic = Periodic String DiffTime Int
  deriving (Eq, Show)

type Mark = (String, LocalTime)

data Habit = Habit String Int
  deriving (Eq, Show)

data Entry = EntryHabit LocalTime Habit
           | EntryPeriodic LocalTime Periodic
           | EntryMark Mark
  deriving (Eq, Show)

entryTime :: Entry -> LocalTime
entryTime (EntryHabit t _) = t
entryTime (EntryPeriodic t _) = t
entryTime (EntryMark (_, t)) = t

-- | Class of things which can be named with a string.
class Nameable s where
  getName :: s -> String

instance Nameable Habit where
  getName (Habit s _) = s

instance Nameable Periodic where
  getName (Periodic s _ _) = s

-- * Some random handy time functions.

l2u :: LocalTime -> UTCTime
l2u = localTimeToUTC utc

diffLocalTime :: LocalTime -> LocalTime -> DiffTime
diffLocalTime a b = realToFrac $ diffUTCTime (l2u a) (l2u b)

addLocalTime :: DiffTime -> LocalTime -> LocalTime
addLocalTime d a = utcToLocalTime utc $ addUTCTime (realToFrac d) (l2u a)
