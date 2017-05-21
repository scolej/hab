module Hab.Game
  ( CharMod(..)
  , CharState(..)
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

import Data.Time
import Hab.Entry
import Hab.GameState
import Hab.Character
import Debug.Trace

-- | Run a set of entries up to a given time.
runEntries
  :: (GameState, CharState) -- ^ Starting state.
  -> [Entry] -- ^ List of entries to run.
  -> LocalTime -- ^ Time to run up to.
  -> (GameState, CharState) -- ^ Resultant state and character.
runEntries (gs, cs) es t =
  let gs' = doTime t $ foldl (flip doEntry) gs es
      mods = dropWhile (\m -> cmDate m <= gsTime gs) (gsMods gs')
  in (gs', deriveCharacter mods cs)

-- | Update the game state with an entry.
doEntry :: Entry -> GameState -> GameState
doEntry e = f . doTime (entryTime e)
  where
    f =
      case e of
        EntryHabit _ h -> gsUpdateItems (ItemHabit h)
        EntryPeriodic _ p -> gsUpdateItems (ItemPeriodic p)
        EntryMark m -> doMark m

doTime :: LocalTime -> GameState -> GameState
doTime = doMissedPeriodics

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
doMarkHabit (Habit name val) t = gsAddCharMods [i] . gsMark name t
  where
    i =
      if val > 0
        then ModExp t name val
        else ModHealth t name val

doMarkPeriodic
  :: Periodic -- ^ The periodic we're marking off.
  -> LocalTime -- ^ The time to mark the periodic off at.
  -> GameState -- ^ Initial game state.
  -> GameState -- ^ The resultant game state.
doMarkPeriodic (Periodic name _ val) t =
  gsAddCharMods [ModExp t name val] . gsMark name t

-- TODO Change this so periodics are active from their creation date, instead of from time they were last marked.
doMissedPeriodics :: LocalTime -> GameState -> GameState
doMissedPeriodics time gs = gsAddCharMods ms gs
  where
    ptime = gsTime gs
    ps = [p | (_, ItemPeriodic p) <- gsItems gs]
    toMod (Periodic n _ v) t = ModHealth t n (-v)
    f p@(Periodic n _ _) =
      let lm = lookup n (gsMarks gs)
      in case lm of
           Nothing -> []
           Just lm_ -> map (toMod p) (missedPeriodics p lm_ (ptime, time))
    ms = concatMap f ps

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
