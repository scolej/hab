module CharDecHealthTest
  ( tests
  ) where

import Data.Time
import Hab.Game
import Test.HUnit

t0 :: LocalTime
t0 = LocalTime (fromGregorian 2016 09 23) (TimeOfDay 10 0 0)

-- | Shortcut for creating health modifications when we don't care about the date or the name.
modh :: Int -> CharMod
modh = ModHealth t0 ""

decHealthSimple :: Test
decHealthSimple =
  let mods = [modh (-5)]
  in TestCase $
     assertEqual
       "Character should decrease in health."
       (CharState (fullHealth - 5) 0 1)
       (deriveCharacter mods blankCharacter)

decHealthDead1 :: Test
decHealthDead1 =
  let mods = [modh (-fullHealth)]
  in TestCase $
     assertEqual
       "Character should be dead."
       (CharDead 0 1)
       (deriveCharacter mods blankCharacter)

decHealthDead2 :: Test
decHealthDead2 =
  let mods = [modh (-fullHealth)]
  in TestCase $
     assertEqual
       "Character should be dead."
       (CharDead 0 1)
       (deriveCharacter mods (CharState 50 0 1))

decHealthDead3 :: Test
decHealthDead3 =
  let mods = [modh (-fullHealth)]
  in TestCase $
     assertEqual
       "Character should be dead but level and experience preserved."
       (CharDead 10 5)
       (deriveCharacter mods (CharState 50 10 5))

tests :: Test
tests =
  TestList [decHealthSimple, decHealthDead1, decHealthDead2, decHealthDead3]
