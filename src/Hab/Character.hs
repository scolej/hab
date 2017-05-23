module Hab.Character where

import Data.Time.LocalTime

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
deriveCharacter = run
  where
    run (m:ms) c = run ms $ doMod m c
    run [] c = c
    doMod (ModExp _ _ dx) (CharState h x l) = normChar $ CharState h (x + dx) l
    doMod (ModHealth _ _ dh) (CharState h x l) =
      normChar $ CharState (h + dh) x l
    doMod _ c@(CharDead _ _) = c

-- | Decide whether or not a character state represents death.
characterIsDead :: CharState -> Bool
characterIsDead (CharDead _ _) = True
characterIsDead _ = False
