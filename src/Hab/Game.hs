module Hab.Game
  ( CharMod(..)
  , CharState(..)
  , GameState(..)
  , Item(..)
  , blankCharacter
  , blankState
  , charSummary
  , deriveCharacter
  , fullHealth
  , lvlExp
  , normChar
  , runEntries
  ) where

import Data.List
import Data.List.Utils
import Data.Ord
import Data.Time
import Hab.Entry

-- | Character state.
data CharState
  = CharState Int -- ^ Health.
              Int -- ^ Experience.
              Int -- ^ Level.
  | CharDead Int -- ^ Experience.
             Int -- ^ Level.
  deriving (Eq, Show)

charSummary :: CharState -> String
charSummary (CharState h x l) =
  "Character is level " ++
  show l ++
  " with " ++
  show x ++ "/" ++ show (lvlExp l) ++ " experience and " ++ show h ++ " health."
charSummary (CharDead x l) =
  "Character is dead at level " ++
  show l ++ " with " ++ show x ++ "/" ++ show (lvlExp l) ++ " experience."

-- | Amount of experience needed to complete a level. Levels start from 1.
lvlExp :: Int -> Int
lvlExp l = head $ drop (l - 1) [10,20 ..]

fullHealth :: Int
fullHealth = 50

blankCharacter :: CharState
blankCharacter = CharState fullHealth 0 1

-- | A modification to the character data.
data CharMod
  = ModExp LocalTime
           String
           Int -- ^ Increment / decrement experience.
  | ModHealth LocalTime
              String
              Int -- ^ Increment / decrement health.
  deriving (Eq)

instance Show CharMod where
  show (ModExp t s v) = unwords ["exp   ", show t, show s, show v]
  show (ModHealth t s v) = unwords ["health", show t, show s, show v]

cmDate :: CharMod -> LocalTime
cmDate (ModExp d _ _) = d
cmDate (ModHealth d _ _) = d

normChar :: CharState -> CharState
normChar c@(CharState h x l)
  | h <= 0 = CharDead x l
  | x >= lvlExp l = normChar $ CharState fullHealth (x - lvlExp l) (l + 1)
  | x < 0 = error "Experience less < 0 !!?? Should this happen?"
  | otherwise = c

deriveCharacter
  :: [CharMod] -- ^ List of character modifications to apply.
  -> CharState -- ^ Starting character state.
  -> CharState -- ^ New character state.
deriveCharacter mods c0 = run mods c0
  where
    run (m:ms) c = run ms $ doMod m c
    run [] c = c
    doMod (ModExp _ _ dx) (CharState h x l) = normChar $ CharState h (x + dx) l
    doMod (ModHealth _ _ dh) (CharState h x l) =
      normChar $ CharState (h + dh) x l
    doMod _ c@(CharDead _ _) = c

-- | Value to store the game state as it evolves over time.
data GameState = GameState
  { gsItems :: [(String, Item)] -- ^ List of named items that can be checked out.
  , gsMarks :: [(String, LocalTime)] -- ^ Association list from item names to the last time that they were checked off.
  , gsMods :: [CharMod] -- ^ List of all modifications to the character that have happened over time.
  , gsTime :: LocalTime -- ^ The time that this state is relevant for. Should always contain the time for the most recent modification.
  } deriving (Eq, Show)

-- TODO Do we actually need this type?
data Item
  = ItemHabit Habit
  | ItemPeriodic Periodic
  deriving (Eq, Show)

-- TODO Better way to do this?
instance Nameable Item where
  getName (ItemHabit x) = getName x
  getName (ItemPeriodic x) = getName x

-- | Empty game state. TODO Not sure what's going on with this date.
blankState :: GameState
blankState =
  GameState [] [] [] $ LocalTime (fromGregorian 2016 1 1) (TimeOfDay 0 0 0)

-- | Run a set of entries up to a given time.
runEntries
  :: (GameState, CharState) -- ^ Starting state.
  -> [Entry] -- ^ List of entries to run.
  -> LocalTime -- ^ Time to run up to.
  -> (GameState, CharState) -- ^ Resultant state and character.
runEntries (gs, cs) es t =
  let gs' = doTime t $ foldl (flip doEntry) gs es
      mods = takeWhile (\m -> cmDate m > gsTime gs) $ gsMods gs'
  in (gs', deriveCharacter mods cs)

-- | Update the game state with an entry.
doEntry :: Entry -> GameState -> GameState
doEntry e = f . doTime (entryTime e)
  where
    f gs =
      case e of
        EntryHabit _ h -> updateItems gs (ItemHabit h)
        EntryPeriodic _ p -> updateItems gs (ItemPeriodic p)
        EntryMark m -> doMark m gs

doTime :: LocalTime -> GameState -> GameState
doTime = doMissedPeriodics

-- | Update the items in the game state with a new item.
updateItems
  :: GameState -- ^ Current game state.
  -> Item -- ^ The new item.
  -> GameState -- ^ Game state with the new item.
updateItems gs i =
  gs
  { gsItems = addToAL (gsItems gs) (getName i) i
  }

doMark :: Mark -> GameState -> GameState
doMark (name, t) gs = u gs
  where
    item = lookup name (gsItems gs)
    u =
      case item of
        Just (ItemHabit h) -> doMarkHabit h t
        Just (ItemPeriodic p) -> doMarkPeriodic p t
        Nothing ->
          error $ "Item was not found in list of active items: " ++ show name

doMarkHabit :: Habit -> LocalTime -> GameState -> GameState
doMarkHabit (Habit name val) t gs =
  let i =
        if val > 0
          then ModExp t name val
          else ModHealth t name val
  in gs
     { gsMarks = addToAL (gsMarks gs) name t
     , gsMods = i : gsMods gs
     , gsTime = t
     }

doMarkPeriodic
  :: Periodic -- ^ The periodic we're marking off.
  -> LocalTime -- ^ The time to mark the periodic off at.
  -> GameState -- ^ Initial game state.
  -> GameState -- ^ The resultant game state.
doMarkPeriodic (Periodic name _ val) t gs =
  let x = ModExp t name val
  in gs
     { gsMarks = addToAL (gsMarks gs) name t
     , gsMods = x : gsMods gs
     , gsTime = t
     } -- TODO We need a function to do this time updating for us. Is going to be repeated everywhere.

-- TODO Change this so periodics are active from their creation date, instead of from time they were last marked.
doMissedPeriodics :: LocalTime -> GameState -> GameState
doMissedPeriodics time gs =
  let ptime = gsTime gs
      ps =
        [ p
        | (_, ItemPeriodic p) <- gsItems gs ]
      toMod (Periodic n _ v) t = ModHealth t n (-v)
      f p@(Periodic n _ _) =
        let lm = lookup n (gsMarks gs)
        in case lm of
             Nothing -> []
             Just lm_ -> map (toMod p) (missedPeriodics p lm_ (ptime, time))
      ms = sortBy (comparing $ Down . cmDate) $ concatMap f ps
  in gs
     { gsMods = ms ++ gsMods gs
     , gsTime = time
     }

-- | Find the points in time at which a periodic was missed.
missedPeriodics
  :: Periodic
  -> LocalTime -- ^ Last time the periodic was marked off.
  -> (LocalTime, LocalTime) -- ^ Window to filter to.
  -> [LocalTime] -- ^ List of missed times.
missedPeriodics (Periodic _ p _) lm (t1, t2) =
  let run t = t : run (addLocalTime p t)
      ps = run lm
  in takeWhile (< t2) . dropWhile (<= t1) $ ps
