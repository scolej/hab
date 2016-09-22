import Test.HUnit
import Game
import Entry
import Data.Time

dayDiff :: DiffTime
dayDiff = 24 * 60 * 60

missedPeriodic1 :: Test
missedPeriodic1 =
  let t1 = LocalTime (fromGregorian 2016 09 23) (TimeOfDay 10 0 0)
      t2 = addLocalTime dayDiff t1
      t3 = addLocalTime 1 t2
      ps = [ EntryPeriodic t1 (Periodic "testp" dayDiff 1)
           , EntryMark ("testp", t1)
           ]
      (gs', _) = runEntries blankState ps t3
  in TestCase $
     assertEqual "There should be a modification for one missed periodic."
     [ ModHealth t2 "testp" (-1) -- Damage for the miss.
     , ModExp t1 "testp" 1 -- Experience for the initial check off.
     ]
     (gsMods gs')

missedPeriodic2 :: Test
missedPeriodic2 =
  let t1 = LocalTime (fromGregorian 2016 09 23) (TimeOfDay 10 0 0)
      t2 = addLocalTime dayDiff t1
      t3 = addLocalTime dayDiff t2
      t4 = addLocalTime 1 t3
      ps = [ EntryPeriodic t1 (Periodic "testp" dayDiff 1)
           , EntryMark ("testp", t1)
           ]
      (gs', _) = runEntries blankState ps t4
  in TestCase $
     assertEqual "There should be a modification for two missed periodics."
     [ ModHealth t3 "testp" (-1) -- Damage for the miss.
     , ModHealth t2 "testp" (-1) -- Damage for the miss.
     , ModExp t1 "testp" 1 -- Experience for the initial check off.
     ]
     (gsMods gs')

main :: IO Counts
main = runTestTT $ TestList [ missedPeriodic1
                            , missedPeriodic2
                            ]
