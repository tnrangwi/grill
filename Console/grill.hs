#!/usr/bin/runghc
-- | This file implements a simple (ugly) console interface. It does
-- only suit for some simple tests and demonstrations.
-- 
-- | Author: Thorsten Rangwich. See file <../LICENSE> for details.

import qualified System
import qualified System.Exit as Exit
import qualified System.Console.GetOpt as GetOpt
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

import qualified System.IO as FileIO

import qualified FormulaEngine.Parse as Parse

import qualified Data.Sheet as Sheet

import Data.String.Utils as StringUtils

-- | String used to mark an argument within the properties.
argument :: String
argument = ""

newtype Properties = Prop { prop :: [(String, String)] }
instance Show Properties where
    show props = let raw = prop props
                     fa (x,_) = if x == argument then True else False
                     fp = not . fa
                     as = map snd (filter fa raw)
                     ps = filter fp raw
                 in
                   "Properties:\n" ++ (concatMap (\(k,v) -> ("   " ++ k ++ ":" ++ v ++ "\n")) ps) ++
                         "Files:\n" ++ (concatMap (\v -> "   " ++ v ++ "\n") as)

-- | Helper function to retrieve argument or property from the properties list.
getProp' :: String
        -> Properties
        -> [String]
getProp' n = map snd . filter (\(k,_) -> if k == n then True else False) . prop

-- | Get a property (list, if given more than once). Does not return special key used as
-- marker for arguments instead of properties.
getProp :: String
        -> Properties
        -> [String]
getProp n p = if n == argument then [] else getProp' n p

-- | Retrieve a boolean flag from within the properties.
getFlag :: String
        -> Properties
        -> Bool
getFlag f = elem (f,"") . prop

-- | Retrieve the argument list from within the properties. These are marked with a special property key.
getArguments :: Properties
             -> [String]
getArguments = getProp' argument


-- | Put property name / value into tuple
mapOption :: String -- ^ Property name
          -> String -- ^ Property value
          -> (String, String)
mapOption = (,)

-- | Put flag with default into tuple
mapFlag :: String -- ^ Flag name
        -> (String, String)
mapFlag = (`mapOption` [])

-- | Command line option description
options :: [GetOpt.OptDescr (String, String)]
options =
    [
     GetOpt.Option ['v','?'] ["version"]
               (GetOpt.NoArg (mapFlag "ShowVersion"))
               "show version",

     GetOpt.Option ['c'] ["console"]
               (GetOpt.NoArg (mapFlag "Console"))
               "no GUI - use console",

     GetOpt.Option ['d'] ["debug"]
               (GetOpt.OptArg (mapOption "DebugLevel" . Maybe.fromMaybe "0") "0|1|2")
               "debug on - pass 1 or 2 to increase level",

     GetOpt.Option ['f'] ["output-filter"]
               (GetOpt.ReqArg (mapOption "OutputFilter") "TSV|CSV|PP")
               "Output filter - TSV, CSV, PP",

     GetOpt.Option ['o'] ["dump-file"]
               (GetOpt.NoArg (mapFlag "Dump"))
               "output contents and exit"
    ]


-- | Parse options and arguments into properties.
parseOptions :: [String] -- ^ Command line arguments
             -> IO Properties -- ^ Parsed properties
parseOptions argv = do
  prog <- System.getProgName
  case GetOpt.getOpt GetOpt.Permute options argv of
    (opts, args, [])   -> return $ Prop $ opts ++ map (\x -> (".", x)) args
    (_, _, errs) -> ioError (userError (concat errs ++ GetOpt.usageInfo header options))
        where header = "Usage: " ++ prog ++ " [options]" 

-- | Print message, wait for return to display it
showMessage :: String -> IO ()
showMessage m = do
  putStr $ m ++ "\nPress <Enter> to continue\n"
  FileIO.getLine
  return ()

-- | Console main loop: Print command line keys, read command and execute it
consoleLoop :: Properties -> Sheet.RawSheet -> IO ()
consoleLoop props sheet = do
  putStr ['\n' | _ <- [0..25]]
  putStr "[D]ump  [L]oad  [S]ave [E]dit cell [Q]uit\n"
  command <- FileIO.getLine >>= return . StringUtils.strip
  case if length command > 0 then command!!0 else ' ' of
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
                      let sheetName = names!!0
                      stream <- FileIO.readFile (names!!0)
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
  argv <- System.getArgs
  props <- parseOptions argv
  putStr $ "Options:\n" ++ show props
  sheet <- loadSheet (getArguments props)
  consoleLoop props sheet
  putStr "Exiting grill...\n"
