#!/usr/bin/runghc
-- | This file implements a simple (ugly) console interface. It does
-- only suit for some simple tests and demonstrations.
-- 
-- | Author: Thorsten Rangwich. See file <../LICENSE> for details.

import qualified System
import qualified System.Console.GetOpt as GetOpt
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe

newtype Properties = Prop { prop :: [(String, String)] }
instance Show Properties where
    show props = let raw = prop props
                     fa (x,_) = if x == "." then True else False
                     fp = not . fa
                     as = map snd (filter fa raw)
                     ps = filter fp raw
                 in
                   "Properties:\n" ++ (concatMap (\(k,v) -> ("   " ++ k ++ ":" ++ v ++ "\n")) ps) ++
                         "Files:\n" ++ (concatMap (\v -> "   " ++ v ++ "\n") as)


getProp' :: String
        -> Properties
        -> [String]
getProp' n = map snd . filter (\(k,_) -> if k == n then True else False) . prop

getProp n p = if n == "." then [] else getProp' n p
           
getFlag :: String
        -> Properties
        -> Bool
getFlag f = elem (f,"") . prop 

getArguments :: String
             -> Properties
             -> [String]
getArguments p = getProp' "."


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

consoleLoop :: IO ()
consoleLoop = do
  putStr ['\n' | _ <- [0..25]]
  putStr "[D]ump  [L]oad  [S]ave [E]dit cell"  

-- | Startup ugly command interface
main :: IO ()

main = do
  argv <- System.getArgs
  props <- parseOptions argv
  putStr $ "Options:\n" ++ show props
  putStr "Exiting grill..."
