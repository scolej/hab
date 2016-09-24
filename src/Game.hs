module Game ( CharMod (..)
            , CharState (..)
            , GameState (..)
            , Item (..)
            , blankState
            , runEntries
            ) where

import Data.List
import Data.List.Utils
import Data.Ord
import Data.Time
import Entry
import Debug.Trace

-- | Character state.
data CharState = CharState { csHealth :: Int
                           , csExp :: Int
                           , csLevel :: Int
                           }
  deriving Show

-- | A modification to the character data.
data CharMod = ModExp LocalTime String Int    -- ^ Increment / decrement experience.
             | ModHealth LocalTime String Int -- ^ Increment / decrement health.
  deriving (Eq)

instance Show CharMod where
  show (ModExp t s v) = unwords ["exp   ", show t, show s, show v]
  show (ModHealth t s v) = unwords ["health", show t, show s, show v]

cmDate :: CharMod -> LocalTime
cmDate (ModExp d _ _) = d
cmDate (ModHealth d _ _) = d

deriveCharacter :: GameState -> CharState
deriveCharacter gs = CharState life ex 1
  where es = gsMods gs
        hmods = [x | ModHealth _ _ x <- es]
        emods = [x | ModExp    _ _ x <- es]
        life = sum hmods
        ex = sum emods

-- | Value to store the game state as it evolves over time.
data GameState = GameState { gsItems :: [(String, Item)]      -- ^ List of named items that can be checked out.
                           , gsMarks :: [(String, LocalTime)] -- ^ Association list from item names to the last time that they were checked off.
                           , gsMods :: [CharMod]              -- ^ List of all modifications to the character that have happened over time.
                           }
  deriving Show

-- TODO Do we actually need this type?
data Item = ItemHabit    Habit
          | ItemPeriodic Periodic
  deriving Show

-- TODO Better way to do this?
instance Nameable Item
  where getName (ItemHabit    x) = getName x
        getName (ItemPeriodic x) = getName x

-- | Empty game state.
blankState :: GameState
blankState = GameState [] [] []

-- | Run a set of entries up to a given time.
runEntries :: GameState              -- ^ Starting state.
           -> [Entry]                -- ^ List of entries to run.
           -> LocalTime              -- ^ Time to run up to.
           -> (GameState, CharState) -- ^ Resultant state and character.
runEntries gs es t =
  let gs' = doTime t $ foldl (flip doEntry) gs es
  in (gs', deriveCharacter gs')

-- | Update the game state with an entry.
doEntry :: Entry -> GameState -> GameState
doEntry e = f . doTime (entryTime e)
  where f gs = case e of
              EntryHabit _ h    -> updateItems gs (ItemHabit h)
              EntryPeriodic _ p -> updateItems gs (ItemPeriodic p)
              EntryMark m       -> doMark m gs

doTime :: LocalTime -> GameState -> GameState
doTime = doMissedPeriodics

-- | Update the items in the game state with a new item.
updateItems :: GameState -- ^ Current game state.
            -> Item      -- ^ The new item.
            -> GameState -- ^ Game state with the new item.
updateItems gs i = gs { gsItems = addToAL (gsItems gs) (getName i) i }

doMark :: Mark -> GameState -> GameState
doMark (name, t) gs = u gs
  where
    item = lookup name (gsItems gs)
    u = case item of
          Just (ItemHabit h)    -> doMarkHabit h t
          Just (ItemPeriodic p) -> doMarkPeriodic p t
          Nothing               -> error $ "Item was not found in list of active items: " ++ show name

doMarkHabit :: Habit -> LocalTime -> GameState -> GameState
doMarkHabit (Habit name val) t gs =
  let i = if val > 0
          then ModExp t name val
          else ModHealth t name val
  in gs { gsMarks = addToAL (gsMarks gs) name t
        , gsMods = i : gsMods gs }

doMarkPeriodic :: Periodic  -- ^ The periodic we're marking off.
               -> LocalTime -- ^ The time to mark the periodic off at.
               -> GameState -- ^ Initial game state.
               -> GameState -- ^ The resultant game state.
doMarkPeriodic (Periodic name _ val) t gs =
  let x = ModExp t name val
  in gs { gsMarks = addToAL (gsMarks gs) name t
        , gsMods = x : gsMods gs }

doMissedPeriodics :: LocalTime -> GameState -> GameState
doMissedPeriodics time gs =
  let (_, ptime) = maximumBy (comparing snd) (gsMarks gs) -- Most recent mark. Use this as t1 for filtering missed periodics.
      ps = [p | (_, ItemPeriodic p) <- gsItems gs ]
      toMod (Periodic n _ v) t = ModHealth t n (-v)
      f p@(Periodic n _ _) = let lm = lookup n (gsMarks gs)
                             in case lm of Nothing -> []
                                           Just lm_ -> map (toMod p) (missedPeriodics p lm_ (ptime, time))
      ms = sortBy (comparing $ Down . cmDate) $ concatMap f ps
  in gs { gsMods = ms ++ gsMods gs }

-- | Find the points in time at which a periodic was missed.
missedPeriodics :: Periodic
                -> LocalTime              -- ^ Last time the periodic was marked off.
                -> (LocalTime, LocalTime) -- ^ Window to filter to.
                -> [LocalTime]            -- ^ List of missed times.
missedPeriodics (Periodic _ p _) lm (t1, t2) =
  let run t = t : run (addLocalTime p t)
      ps = run lm
  in takeWhile (< t2) . dropWhile (<= t1) $ ps
               
