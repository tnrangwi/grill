#!/usr/bin/runghc
-- | This file demonstrates parsing a whole sheet and display the contents.
--
-- | Author: Thorsten Rangwich. See file <../LICENSE> for details.

import qualified System
import qualified System.Exit as Exit
import qualified System.IO as FileIO
import qualified Control.Monad as M

-- | Tell error and exit from main.
exitMain :: String -- ^ Error message
         -> IO () -- Just exit, no return value
exitMain s = FileIO.putStrLn s >> Exit.exitFailure

-- | Check condition and exit with given error message if condition is True
testExit :: Bool -- ^ Condition to test
         -> String -- ^ Error message
         -> IO () -- ^ Either returns nothing or exits (returning nothing)
testExit c = M.when c . exitMain

-- | Main
main :: IO ()
main = do
  argv <- System.getArgs
  testExit (length argv /= 1) "Usage: display_sheet.hs <filename>"
  let sheetFile = head argv
  rawSheet <- FileIO.readFile sheetFile
  putStr $ "Parsing raw sheet:\n===\n" ++ rawSheet ++ "===\n"
  -- Parse contents using the enhanced parser for a whole sheet - TODO!
