import Test.HUnit
import Game
import Entry
import Data.Time

t0 :: LocalTime
t0 = LocalTime (fromGregorian 2016 09 23) (TimeOfDay 10 0 0)

hourDiff :: DiffTime
hourDiff = 60 * 60

dayDiff :: DiffTime
dayDiff = 24 * hourDiff

(/+) :: LocalTime -> DiffTime -> LocalTime
infixl 1 /+
(/+) = flip addLocalTime

missedPeriodic1 :: Test
missedPeriodic1 =
  let ps = [ EntryPeriodic t0 (Periodic "testp" dayDiff 1)
           , EntryMark ("testp", t0)
           ]
      (gs', _) = runEntries blankState ps (t0 /+ dayDiff + 1)
  in TestCase $ assertEqual "Miss one periodic."
     [ ModHealth (t0 /+ dayDiff) "testp" (-1)
     , ModExp t0 "testp" 1
     ]
     (gsMods gs')

missedPeriodic2 :: Test
missedPeriodic2 =
  let ps = [ EntryPeriodic t0 (Periodic "testp" dayDiff 1)
           , EntryMark ("testp", t0)
           ]
      (gs', _) = runEntries blankState ps (t0 /+ 2 * dayDiff + 1)
  in TestCase $ assertEqual "Miss the same periodic twice."
     [ ModHealth (t0 /+ dayDiff * 2) "testp" (-1)
     , ModHealth (t0 /+ dayDiff) "testp" (-1)
     , ModExp t0 "testp" 1
     ]
     (gsMods gs')

multiMissedPeriodic1 :: Test
multiMissedPeriodic1 =
  let ps = [ EntryPeriodic t0 (Periodic "testp1" dayDiff 1)
           , EntryPeriodic t0 (Periodic "testp2" dayDiff 2)
           , EntryMark ("testp1", t0)
           , EntryMark ("testp2", t0)
           ]
      (gs', _) = runEntries blankState ps (t0 /+ dayDiff + 1)
  in TestCase $ assertEqual "Miss two periodics."
     [ ModHealth (t0 /+ dayDiff) "testp2" (-2) -- Damage for the miss.
     , ModHealth (t0 /+ dayDiff) "testp1" (-1)
     , ModExp t0 "testp2" 2 -- Experience for the initial check off.
     , ModExp t0 "testp1" 1
     ]
     (gsMods gs')

missedPeriodicMultiMark :: Test
missedPeriodicMultiMark =
  let ps = [ EntryPeriodic t0 (Periodic "testp" dayDiff 1)
           , EntryMark ("testp", t0)
           , EntryMark ("testp", t0 /+ hourDiff)
           ]
      (gs', _) = runEntries blankState ps $ t0 /+ hourDiff /+ dayDiff + 1
  in TestCase $ assertEqual "Mark twice and miss one."
     [ ModHealth (t0 /+ hourDiff /+ dayDiff) "testp" (-1)
     , ModExp (t0 /+ hourDiff) "testp" 1
     , ModExp t0 "testp" 1
     ]
     (gsMods gs')

differentMissedPeriodic :: Test
differentMissedPeriodic =
  let ps = [ EntryPeriodic t0 (Periodic "testp3" (dayDiff * 3) 3)
           , EntryPeriodic t0 (Periodic "testp2" (dayDiff * 2) 2)
           , EntryMark ("testp3", t0)
           , EntryMark ("testp2", t0)
           ]
      (gs', _) = runEntries blankState ps $ t0 /+ 4 * dayDiff + 1
  in TestCase $ assertEqual "One is missed twice, the other once."
     [ ModHealth (t0 /+ 4 * dayDiff) "testp2" (-2)
     , ModHealth (t0 /+ 3 * dayDiff) "testp3" (-3)
     , ModHealth (t0 /+ 2 * dayDiff) "testp2" (-2)
     , ModExp t0 "testp2" 2
     , ModExp t0 "testp3" 3
     ]
     (gsMods gs')

missedPeriodicWithMultiHabitMark :: Test
missedPeriodicWithMultiHabitMark =
  let ps = [ EntryHabit t0 (Habit "habit" 1)
           , EntryPeriodic t0 (Periodic "per" dayDiff 2)
           , EntryMark ("per", t0)
           , EntryMark ("habit", t0 /+ dayDiff + 1)
           , EntryMark ("habit", t0 /+ dayDiff + 1)
           ]
      (gs', _) = runEntries blankState ps $ t0 /+ dayDiff + 1
  in TestCase $ assertEqual "Habit is marked twice but periodic is missed only once."
     [ ModExp (t0 /+ dayDiff + 1) "habit" 1
     , ModExp (t0 /+ dayDiff + 1) "habit" 1
     , ModHealth (t0 /+ dayDiff) "per" (-2)
     , ModExp t0 "per" 2
     ]
     (gsMods gs')

main :: IO Counts
main = runTestTT $ TestList [ missedPeriodic1
                            , missedPeriodic2
                            , multiMissedPeriodic1
                            , missedPeriodicMultiMark
                            , differentMissedPeriodic
                            , missedPeriodicWithMultiHabitMark
                            ]
