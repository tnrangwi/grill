-- | This file implements command line tools for startup of grill.
-- In fact it currently is a general command line option parser.
-- 
-- | Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Console.CommandLine
(
 -- * Accessing command line options.
 Properties,
 getFlag,
 getProp,
 getArguments,
 -- * Parsing command line
 PropertyType(..),
 Argument(..),
 parseOptions,
 parseCmdLine
)
where

import qualified System
import qualified System.Console.GetOpt as GetOpt

import qualified Data.Maybe as Maybe


-- Properties implementation

-- | Type storing command line properties. Properties are named by string
-- and may occur more than once. The empty string marks command line
-- arguments.
newtype Properties = Prop { prop :: [(String, String)] }
instance Show Properties where
    show props = let raw = prop props
                     fa (x,_) = x == argument
                     fp = not . fa
                     as = map snd (filter fa raw)
                     ps = filter fp raw
                 in
                   "Properties:\n" ++ concatMap (\(k,v) -> ("   " ++ k ++ ":" ++ v ++ "\n")) ps ++
                         "Files:\n" ++ concatMap (\v -> "   " ++ v ++ "\n") as

-- | Empty string used to mark an argument instead of a property within the properties.
argument :: String
argument = ""

-- | Helper function to retrieve argument or property from the properties list.
-- This is implementation and is not exported.
getKey :: String
       -> Properties
       -> [String]
getKey n = map snd . filter (\(k,_) -> k == n) . prop

-- | Get a property (list, if given more than once). Use getArguments to retrieve
-- command line arguments instead of command line options.
getProp :: String
        -> Properties
        -> [String]
getProp n p = if n == argument then [] else getKey n p

-- | Retrieve a command line option without an argument.
getFlag :: String
        -> Properties
        -> Bool
getFlag f = elem (f,"") . prop

-- | Retrieve the command line arguments from the properties.
getArguments :: Properties
             -> [String]
getArguments = getKey argument

-- Command line parsing

-- | Helper function to put property name / value into tuple
option :: String -- ^ Property name
          -> String -- ^ Property value
          -> (String, String)
option = (,)

-- | Put flag with default into tuple
flag :: String -- ^ Flag name
        -> (String, String)
flag = (`option` [])


-- specifying command line

-- | Data constructor to specify type of command line option.
data PropertyType = Flag -- ^ Command line option is a flag without parameter.
                  | OptOpt String String -- ^ Command line option is a flag with optional parameter description
                                         -- and default value.
                  | ReqOpt String -- ^ Command line option is a flag with a required parameter description.

-- | Data constructor to specify one command line argument.
data Argument = Argument {
      shortNames :: String -- ^ String with all short option characters matching this option / flag.
      ,longNames :: [String] -- ^ List of long option strings matching this option / flag.
      ,description :: String -- ^ Description of this command line option.
      ,propertyName :: String -- ^ Name of the property to identify this option within properties.
      ,propertyType :: PropertyType -- ^ Type of this command line option.
    }


-- | Convert option specification given as list to option specification useable by GetOpt.
optionSpec2GetOpt :: [Argument]
                  -> [GetOpt.OptDescr (String, String)]
optionSpec2GetOpt [] = []
optionSpec2GetOpt (x:xs) = convert x : optionSpec2GetOpt xs --FIXME: Efficient?
    where convert v = GetOpt.Option
                      (shortNames v)
                      (longNames v)
                      (convertType (propertyName v) (propertyType v))
                      (description v)
          convertType n t = case t of
                              Flag -> GetOpt.NoArg (flag n)
                              OptOpt desc dflt -> GetOpt.OptArg (option n . Maybe.fromMaybe dflt) desc
                              ReqOpt desc -> GetOpt.ReqArg (option n) desc


-- | Parse options and arguments into properties.
parseOptions :: [Argument] -- ^ Options specification
             -> [String] -- ^ Command line arguments
             -> IO Properties -- ^ Parsed properties
parseOptions optionSpec argv = do
  let options = optionSpec2GetOpt optionSpec
  prog <- System.getProgName
  case GetOpt.getOpt GetOpt.Permute options argv of
    (opts, args, [])   -> return $ Prop $ opts ++ map (\x -> ("", x)) args
    (_, _, errs) -> ioError (userError (concat errs ++ GetOpt.usageInfo header options))
        where header = "Usage: " ++ prog ++ " [options]" 

-- | Parse command line and return properties for further processing.
-- further options for debugging are processed.
parseCmdLine :: [Argument] -- ^ Options speficication
             -> IO Properties -- ^ Parsed object to be used for queries
parseCmdLine options = System.getArgs >>= parseOptions options

