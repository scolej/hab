module Hab.GameState
  ( GameState
  , gsItems
  , gsMarks
  , gsMods
  , gsTime
  , blankState
  , Item (..)
  , gsAddCharMods
  , gsUpdateItems
  , gsMark
  ) where

import Data.Time.LocalTime
import Data.Time.Calendar
import Hab.Entry
import Hab.Character
import Data.Ord
import Data.List
import Data.List.Utils

-- | Value to store the game state as it evolves over time.
data GameState = GameState
  { _gsItems :: [(String, Item)] -- ^ List of named items that can be checked out.
  , _gsMarks :: [(String, LocalTime)] -- ^ Association list from item names to the last time that they were checked off.
  , _gsMods :: [CharMod] -- ^ List of all modifications to the character that have happened over time.
  , _gsTime :: LocalTime -- ^ The time that this state is relevant for. Should always contain the time for the most recent modification.
  } deriving (Eq, Show)

-- | Add new character modifications into the game state and update
-- the latest time if necessary. Modifications can be added in any
-- time order.
gsAddCharMods :: [CharMod] -> GameState -> GameState
gsAddCharMods mods gs =
  let mods' = sortBy (comparing cmDate) (_gsMods gs ++ mods)
  in gs
     { _gsTime = maximum (map cmDate mods')  -- Use whichever time is latest.
     , _gsMods = mods'
     }

-- | Update the items in the game state with a new item.
gsUpdateItems
  :: Item -- ^ The new item.
  -> GameState -- ^ Current game state.
  -> GameState -- ^ Game state with the new item.
gsUpdateItems i gs =
  gs
  { _gsItems = addToAL (gsItems gs) (getName i) i
  }

-- | Mark off an item at a time.
gsMark :: String -> LocalTime -> GameState -> GameState
gsMark name t gs = gs {_gsMarks = addToAL (_gsMarks gs) name t}

-- Make copies of the record selectors so that importing
-- code can't use them to update the state.
-- TODO Better way to do this?
gsItems = _gsItems
gsMarks = _gsMarks
gsMods = _gsMods
gsTime = _gsTime

-- | Empty game state.
blankState :: GameState
blankState =
  -- TODO Not sure what's going on with this date.
  GameState [] [] [] $ LocalTime (fromGregorian 2016 1 1) (TimeOfDay 0 0 0)


-- TODO Do we actually need this type?
data Item
  = ItemHabit Habit
  | ItemPeriodic Periodic
  deriving (Eq, Show)

-- TODO Better way to do this?
instance Nameable Item where
  getName (ItemHabit x) = getName x
  getName (ItemPeriodic x) = getName x

