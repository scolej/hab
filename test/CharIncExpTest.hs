module CharIncExpTest
  ( tests
  ) where

import Data.Time
import Hab.Game
import Test.HUnit

t0 :: LocalTime
t0 = LocalTime (fromGregorian 2016 09 23) (TimeOfDay 10 0 0)

-- | Shortcut for creating health modifications when we don't care about the date or the name.
modx :: Int -> CharMod
modx = ModExp t0 ""

lvlExp1 :: Test
lvlExp1 =
  TestCase $
  assertEqual "Test level experience." [10, 20, 30] (map lvlExp [1, 2, 3])

incExpSimple :: Test
incExpSimple =
  let mods = [modx (lvlExp 1)] -- Enough experience to bump to next level.
  in TestCase $
     assertEqual
       "Character should advance to level 2."
       (CharState fullHealth 0 2)
       (deriveCharacter mods blankCharacter)

incExpSimpleHeal :: Test
incExpSimpleHeal =
  let mods = [modx (lvlExp 1)] -- Enough experience to bump to next level.
  in TestCase $
     assertEqual
       "Character should advance to level 2."
       (CharState fullHealth 0 2)
       (deriveCharacter mods (CharState 1 0 1))

incExpSimple2 :: Test
incExpSimple2 =
  let mods = [modx (lvlExp 1 + 1)] -- Enough experience to bump to next level.
  in TestCase $
     assertEqual
       "Character should advance to level 2 with a bit left over."
       (CharState fullHealth 1 2)
       (deriveCharacter mods blankCharacter)

incExpHeal :: Test
incExpHeal =
  let mods = [modx (lvlExp 1)] -- Enough experience to bump to next level.
  in TestCase $
     assertEqual
       "Character should advance to level 2."
       (CharState fullHealth 0 2)
       (deriveCharacter mods (CharState 1 0 1))

incExpOverflow :: Test
incExpOverflow =
  let mods = [modx (lvlExp 1 + lvlExp 2)] -- Enough experience to jump 2 levels.
  in TestCase $
     assertEqual
       "Character should advance to level 3."
       (CharState fullHealth 0 3)
       (deriveCharacter mods blankCharacter)

incExpOverflow2 :: Test
incExpOverflow2 =
  let mods = [modx (lvlExp 1 + lvlExp 2 + 1)] -- Enough experience to jump 2 levels.
  in TestCase $
     assertEqual
       "Character should advance to level 3 with a bit left over."
       (CharState fullHealth 1 3)
       (deriveCharacter mods blankCharacter)

tests :: Test
tests =
  TestList
    [ lvlExp1
    , incExpSimple
    , incExpSimple2
    , incExpHeal
    , incExpSimpleHeal
    , incExpOverflow
    , incExpOverflow2
    ]
