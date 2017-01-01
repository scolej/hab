import Test.HUnit
import Game
import Entry
import Data.Time

-- Tests for decreasing character health.
t0 :: LocalTime
t0 = LocalTime (fromGregorian 2016 09 23) (TimeOfDay 10 0 0)

-- | Shortcut for creating health modifications when we don't care about the date or the name.
modh i = ModHealth t0 "" i

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

main :: IO Counts
main =
  runTestTT $
  TestList [decHealthSimple, decHealthDead1, decHealthDead2, decHealthDead3]
