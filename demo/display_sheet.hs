#!/usr/bin/runghc
-- | This file demonstrates parsing a whole sheet and display the contents.
--
-- | Author: Thorsten Rangwich. See file <../LICENSE> for details.

import qualified System
import qualified System.Exit as Exit
import qualified System.IO as FileIO

exitMain :: String -> IO ()
exitMain s = FileIO.putStrLn s >> Exit.exitFailure

testExit :: Bool -> String -> IO ()
testExit c m = if c then exitMain m else return ()

main :: IO ()
main = do
  argv <- System.getArgs
  testExit (length argv /= 1) "Usage: display_sheet.hs <filename>"
  let sheetFile = argv !! 0
  rawSheet <- FileIO.readFile sheetFile
  putStr $ "Parsing raw sheet:\n===\n" ++ rawSheet ++ "===\n"

