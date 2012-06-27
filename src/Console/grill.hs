#!/usr/bin/runghc
-- | This file implements a simple (ugly) console interface. It does
-- only suit for some simple tests and demonstrations.
-- 
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

import qualified System.IO as FileIO
import qualified Data.Char as DChar
import qualified Control.Monad as Monad

import qualified Console.CommandLine as Cmd
import qualified Console.IO as ConIO
import qualified FormulaEngine.Parse as Parse
import qualified Data.Sheet as Sheet
import qualified Procedures.Serialise.Dump as Dump
import qualified Version.Information as Version


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
     Cmd.Argument "v" ["version"] "show version" "ShowVersion" Cmd.Flag
    ,Cmd.Argument "h?" ["help"] "show help" "Help" Cmd.Flag
    ,Cmd.Argument "c" ["console"] "no GUI - use console" "Console" Cmd.Flag
    ,Cmd.Argument "d" ["debug"] "debug on - pass number to increase level" "DebugLevel" (Cmd.OptOpt "1|2|3" "1")
    ,Cmd.Argument "f" ["output-filter"] "Output filter - TSV, CSV, PP" "OutputFilter" (Cmd.ReqOpt "TSV|CSV|PP")
    ,Cmd.Argument "o" ["dump-file"] "output contents and exit" "Dump" Cmd.Flag
    ]

-- | Help string for particular command.
onlineHelp :: String -- ^ The command string to get help for. E.g. "e" for edit.
           -> String
onlineHelp m = "\nGrill help for" ++ m ++
             "\n" ++
             "TODO: write help\n"


-- | Replace that with internal function in Data.Text when switching to Data.Text instead of String.
-- There surely should not be functions like that in this main module.
strip :: String -> String
strip unstripped = walk unstripped [] 0 0
    where walk :: String -- ^ Rest of string to process
               -> String -- ^ Lstripped string. Empty in the beginning, non empty and constant when first chart is found.
               -> Int -- ^ Number of cheracters in lstripped string. Maximum non wghitespace character position.
               -> Int -- ^ Position in lstripped string currently processed.
               -> String -- ^ Result string.
          walk [] s l _ = take l s
          walk (x:xs) [] l n | isWspace x = walk xs [] l n
                             | otherwise = walk xs (x:xs) 1 1
          walk (x:xs) ys l n | isWspace x = walk xs ys l (n + 1)
                             | otherwise = walk xs ys (n + 1) (n + 1)
          isWspace = (`elem` " \t\r\n")

-- | Parse command line from stdin.
getCommandLine :: IO (Char, String) -- ^ Returns (command char, arguments) if OK, (' ', "") if empty or (!, error message)
getCommandLine = do
  commandLine <- fmap strip FileIO.getLine
  let lCmd = length commandLine
  if lCmd == 0
    then return (' ', "")
    else do
      let cmd = DChar.toLower . head $ commandLine
      if lCmd == 1
        then
            return (cmd, "")
        else
            if commandLine!!1 == ' ' then return (cmd, strip . tail $ commandLine) else return ('!', commandLine)


-- | Console main loop: Print command line keys, read command and execute it
-- FIXME: Do not use command line properties. Use something else!
consoleLoop :: Cmd.Properties -> Sheet.Sheet -> IO ()
consoleLoop props sheet = do
  putStr . replicate 25 $ '\n'
  putStr "[C]alculate [D]ump  [L]oad  [S]ave [E]dit cell [H]elp [Q]uit\n"
  -- Design pattern: See fmap remark in Prelude: fmap func (IO x) == (IO x) >>= return . func
  (command, args) <- getCommandLine
  case command of
    'q' -> return ()
    'l' -> loadSheet [args] >>= consoleLoop props
                          --FIXME without type compiler searches for ":: IO a" instead. Why?
    'c' -> (Dump.eval sheet :: IO ()) >> ConIO.showMessage "" >> consoleLoop props sheet
    'd' -> (Dump.dump sheet :: IO ()) >> ConIO.showMessage "" >> consoleLoop props sheet
    's' -> ConIO.showMessage "Save not yet implemented" >> consoleLoop props sheet
    'e' -> case Parse.compileEditCell args of
             Left err -> ConIO.showMessage err >> consoleLoop props sheet
             Right (addr, tree) -> ConIO.showMessage "Cell changed" >> consoleLoop props (Sheet.changeCell addr tree sheet)
    'h' -> ConIO.showMessage (onlineHelp args) >> consoleLoop props sheet
    ' ' -> consoleLoop props sheet
    '!' -> ConIO.showMessage ("Error parsing command line:" ++ args) >> consoleLoop props sheet
    _ -> ConIO.showMessage ("Unrecognised command char:" ++ [command]) >> consoleLoop props sheet

loadSheet :: [String]
          -> IO Sheet.Sheet
loadSheet names = case length names of
                    0 -> ConIO.showMessage "No sheet preloaded" >> return Sheet.emptySheet
                    1 -> do
                      let sheetName = head names
                      stream <- FileIO.readFile (head names)
                      let sheet = Parse.compileSheet stream
                      case sheet of
                        Left msg -> ConIO.showMessage ("Parse error in sheet:" ++ msg) >> return Sheet.emptySheet
                        Right parsedSheet -> ConIO.showMessage ("Sheet loaded:" ++ sheetName) >> return parsedSheet
                    _ -> do
                      putStr "Only one sheet supported"
                      return Sheet.emptySheet


-- | Startup ugly command interface
main :: IO ()

main = do
  props <- Cmd.parseCmdLine options
  let dbLevel = (if null (Cmd.getProp "DebugLevel" props) then "0" else head $ (Cmd.getProp "DebugLevel" props))
  Monad.when (dbLevel >= "1") (putStr $ "Options:\n" ++ show props)
  sheet <- loadSheet (Cmd.getArguments props)
  case True of
    True | (Cmd.getFlag "ShowVersion" props) -> (ConIO.exitMessage ("grill version " ++ Version.versionString))
         | (Cmd.getFlag "Help" props) -> (ConIO.exitMessage (Cmd.helpCmdLine options))
         | (Cmd.getFlag "Dump" props) -> fail "Dump sheet from commmand line is not yet implemented"
         | (Cmd.getFlag "Console" props) -> consoleLoop props sheet
    _ ->   putStrLn "Defaulting to console" >> consoleLoop props sheet
  putStr "Exiting grill...\n"

