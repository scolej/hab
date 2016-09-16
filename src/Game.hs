module Game (CharMod (..)
            , CharacterState (..)
            , followEntries
            ) where

import Data.List.Utils
import Data.Time
import Entry

data CharMod = ModExp LocalTime String Int | ModHealth LocalTime String Int
  deriving Show

data CharacterState = CharacterState { csLife :: Int
                                     , csExp :: Int
                                     , csLevel :: Int
                                     }

data GameStateAcc = GameStateAcc { gsItems :: [(String, Item)]
                                 , gsLastPeriodicCheck :: [(String, LocalTime)]
                                 , gsMods :: [CharMod]
                                 }

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

followEntries :: [Entry] -> [CharMod]
followEntries es = gsMods $ foldl doEntry blankState es

doEntry :: GameStateAcc -> Entry -> GameStateAcc
doEntry gs e =
  case e of EntryHabit h _ -> updateItems gs (ItemHabit h)
            EntryPeriodic p _ -> updateItems gs (ItemPeriodic p)
            EntryMark n t -> doMark gs n t
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
         error $ "Item was not found in list of active items: " ++ show item

doMarkPeriodic :: GameStateAcc -- ^ Initial game state.
               -> Periodic     -- ^ The periodic we're marking off.
               -> LocalTime    -- ^ The time to mark the periodic off at.
               -> GameStateAcc -- ^ The resultant game state.
doMarkPeriodic gs (Periodic name p val) time =
  let lpc = addToAL (gsLastPeriodicCheck gs) name time
      is = undefined
  in gs { gsLastPeriodicCheck = lpc, gsMods = is ++ gsMods gs }
