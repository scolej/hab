module Game (GameState (..)
            , CharacterState (..)
            ) where

import Entry
import Data.Time

data GameState = GameState { gsItems :: [(String, Item)]
                           , gsLastPeriodicCheck :: [(String, LocalTime)]
                           , gsCharState :: CharacterState
                           }

data CharacterState = CharacterState { csLife :: Int
                                     , csExp :: Int
                                     , csLevel :: Int
                                     }
