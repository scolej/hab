import Control.Monad
import Game
import Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ error "Need a file!"

  -- Read in a list of entries
  es <- loadFile (head args)

  -- mapM_ print es
  mapM_ print $ followEntries es
  
  return ()
