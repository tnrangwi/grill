#!/usr/bin/runghc
-- | This file implements a simple (ugly) console interface. It does
-- only suit for some simple tests and demonstrations.
-- 
-- | Author: Thorsten Rangwich. See file <../LICENSE> for details.

import qualified Data.Maybe as Maybe
import Data.String.Utils as StringUtils
import qualified System.IO as FileIO

import qualified Console.CommandLine as Cmd
import qualified FormulaEngine.Parse as Parse
import qualified Data.Sheet as Sheet

-- | Command line option description for grill.
-- List of Option constructor from System.GetOpt taking the following arguments:
-- * String with all characters mapped to that short option
-- * List of strings with all long option words mapped to that option
-- * Argument specification with NoArg / OptArg (optional argument) or ReqArg and name in properties.
-- Use flag for NoArg and option for ReqArg or OptArg with a default value.
-- * Description if user enters invalid command line parameters.
-- FIXME: Using GetOpt here is ugly, a list should be enough.

options :: [Cmd.Argument]
options =
    [
     Cmd.Argument "v?" ["version"] "show version" "ShowVersion" Cmd.Flag
    ,Cmd.Argument "c" ["console"] "no GUI - use console" "Console" Cmd.Flag
    ,Cmd.Argument "d" ["debug"] "debug on - pass number to increase level" "DebugLevel" (Cmd.OptOpt "0|1|2" "0")
    ,Cmd.Argument "f" ["output-filter"] "Output filter - TSV, CSV, PP" "OutputFilter" (Cmd.ReqOpt "TSV|CSV|PP")
    ,Cmd.Argument "o" ["dump-file"] "output contents and exit" "Dump" Cmd.Flag
    ]

-- | Print message, wait for return to display it
showMessage :: String -> IO ()
showMessage m = do
  putStr $ m ++ "\nPress <Enter> to continue\n"
  FileIO.getLine
  return ()

-- | Console main loop: Print command line keys, read command and execute it
-- FIXME: Do not use command line properties. Use something else!
consoleLoop :: Cmd.Properties -> Sheet.RawSheet -> IO ()
consoleLoop props sheet = do
  putStr ['\n' | _ <- [0..25]]
  putStr "[D]ump  [L]oad  [S]ave [E]dit cell [Q]uit\n"
  -- Design pattern: See fmap remark in Prelude: fmap func (IO x) == (IO x) >>= return . func
  command <- fmap StringUtils.strip FileIO.getLine
  case if length command > 0 then head command else ' ' of
    'q' -> return ()
    'l' -> showMessage "Load not yet implemented" >> consoleLoop props sheet
    'd' -> showMessage "Dump not yet implemented" >> consoleLoop props sheet
    's' -> showMessage "Save not yet implemented" >> consoleLoop props sheet
    'e' -> showMessage "Edit not yet implemented" >> consoleLoop props sheet
    ' ' -> consoleLoop props sheet
    otherwise -> showMessage ("Unrecognised command line:" ++ command) >> consoleLoop props sheet

loadSheet :: [String]
          -> IO Sheet.RawSheet
loadSheet names = case length names of
                    0 -> showMessage "No sheet preloaded" >> return Sheet.emptyRawSheet
                    1 -> do
                      let sheetName = head names
                      stream <- FileIO.readFile (head names)
                      let sheet = Parse.compileSheet stream
                      case sheet of
                        Left msg -> showMessage ("Parse error in sheet:" ++ msg) >> return Sheet.emptyRawSheet
                        Right parsedSheet -> showMessage ("Sheet loaded:" ++ sheetName) >> return parsedSheet
                    otherwise -> do
                      putStr "Only one sheet supported"
                      return Sheet.emptyRawSheet


-- | Startup ugly command interface
main :: IO ()

main = do
  props <- Cmd.parseCmdLine options
  putStr $ "Options:\n" ++ show props
  sheet <- loadSheet (Cmd.getArguments props)
  consoleLoop props sheet
  putStr "Exiting grill...\n"
