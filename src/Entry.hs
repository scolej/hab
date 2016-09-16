module Entry ( Habit (..)
             , Periodic (..)
             , Todo (..)
             , Entry (..)
             , Item (..)
             ) where

import Data.Time

data Habit = Habit String Int

data Periodic = Periodic String DiffTime Int

data Todo = Todo String Int

data Item = ItemHabit    Habit
          | ItemPeriodic Periodic
          | ItemTodo     Todo

data Entry = EntryHabit    Habit    LocalTime
           | EntryPeriodic Periodic LocalTime
           | EntryTodo     Todo     LocalTime
           | EntryCheck    String   LocalTime
