-- | This file implements command line IO tools for startup of grill.
-- 
-- | Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Console.IO
(
 -- * Exit
 exitMessage
 -- * Show
 ,showMessage
)
where

import qualified System.Exit as Exit
import qualified System.IO as FileIO

-- | Print message, wait for return to display it
showMessage :: String -> IO ()
showMessage m = do
  putStr $ m ++ "\nPress <Enter> to continue\n"
  _ <- FileIO.getLine
  return ()


-- | Show a final message and exit.
exitMessage :: String -- ^ The message to display before exit
            -> IO () -- ^ Side effect: Exit main program.
exitMessage m = putStrLn m >> Exit.exitSuccess
