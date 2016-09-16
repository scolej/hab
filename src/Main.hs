import System.Environment
import Control.Monad
import Parser

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ error "Need a file!"

  -- Read in a list of entries
  es <- loadFile (head args)
  
  return ()
