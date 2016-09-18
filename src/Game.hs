-- TODO A monster mess. Clean it.
-- advanceStateTime :: GameState -> UTCTime -> GameState

module Game ( CharMod (..)
            , CharacterState (..)
            , GameStateAcc (..)
            , followEntries
            , blankState
            , charFromState
            , Item (..)
            ) where

import Debug.Trace
import Data.List.Utils
import Data.Time
import Entry

data CharMod = ModExp LocalTime String Int | ModHealth LocalTime String Int
  deriving Show

data CharacterState = CharacterState { csLife :: Int
                                     , csExp :: Int
                                     , csLevel :: Int
                                     }
  deriving Show

charFromState :: GameStateAcc -> CharacterState
charFromState gs = CharacterState life ex 1
  where es = gsMods gs
        hmods = [x | ModHealth _ _ x <- es]
        emods = [x | ModExp    _ _ x <- es]
        life = sum hmods
        ex = sum emods

data GameStateAcc = GameStateAcc { gsItems :: [(String, Item)]
                                 , gsLastPeriodicMark :: [(String, LocalTime)]
                                 , gsMods :: [CharMod]
                                 }
  deriving Show

data Item = ItemHabit    Habit
          | ItemPeriodic Periodic
          | ItemTodo     Todo
  deriving Show

-- TODO Better way to do this?
instance Nameable Item
  where getName (ItemHabit    x) = getName x
        getName (ItemPeriodic x) = getName x
        getName (ItemTodo     x) = getName x

blankState :: GameStateAcc
blankState = GameStateAcc [] [] []

followEntries :: GameStateAcc -> LocalTime -> [Entry] -> GameStateAcc
followEntries gs now es =
  let finalState = foldl doEntry gs es
  in finalState { gsMods = calcFinalPeriodics now finalState ++ gsMods finalState }

calcFinalPeriodics :: LocalTime -> GameStateAcc -> [CharMod]
calcFinalPeriodics now gs =
  let f (_, x) = case x of ItemPeriodic p -> [p]
                           _ -> []
      ps = concatMap f (gsItems gs)
  in concatMap (\p@(Periodic n _ _) ->
                  calcPeriodic p (lookup n (gsLastPeriodicMark gs)) now) ps

doEntry :: GameStateAcc -> Entry -> GameStateAcc
doEntry gs e =
  case e of EntryHabit h _ -> updateItems gs (ItemHabit h)
            EntryPeriodic p _ -> updateItems gs (ItemPeriodic p)
            EntryMark (n, t) -> doMark gs n t
            _ -> error $ "Don't know how to handle these entries yet: " ++ show e

-- TODO Lenses?
updateItems :: GameStateAcc -> Item -> GameStateAcc
updateItems gs i = gs { gsItems = addToAL (gsItems gs) (getName i) i }

doMark :: GameStateAcc -> String -> LocalTime -> GameStateAcc
doMark gs name time =
  let item = lookup name (gsItems gs)
  in case item of
       Just (ItemHabit (Habit _ val)) ->
         let i = if val > 0
                 then ModExp time name val
                 else ModHealth time name val
         in gs { gsMods = i : gsMods gs }
       Just (ItemPeriodic p) -> doMarkPeriodic gs p time
       Just _ ->
         error $ "Don't know how to mark this item yet: " ++ show item
       Nothing ->
         error $ "Item was not found in list of active items: " ++ show name

doMarkPeriodic :: GameStateAcc -- ^ Initial game state.
               -> Periodic     -- ^ The periodic we're marking off.
               -> LocalTime    -- ^ The time to mark the periodic off at.
               -> GameStateAcc -- ^ The resultant game state.
doMarkPeriodic gs p@(Periodic name _ val) time =
  let lpc = addToAL (gsLastPeriodicMark gs) name time
      lm = lookup name (gsLastPeriodicMark gs)
      is = calcPeriodic p lm time
      x = ModExp time name val
  in gs { gsLastPeriodicMark = lpc, gsMods = x : is ++ gsMods gs }

calcPeriodic :: Periodic -> Maybe LocalTime -> LocalTime -> [CharMod]
calcPeriodic (Periodic name p val) lastMarked now =
  let u = localTimeToUTC utc
      u' = utcToLocalTime utc
      d = (fromRational . toRational) p :: NominalDiffTime
  in case lastMarked of
       Nothing -> []
       Just lm -> run (addUTCTime d (u lm))
         where run t = if t < u now
                       then ModHealth (u' t) name (-val) : run (addUTCTime d t)
                       else []
