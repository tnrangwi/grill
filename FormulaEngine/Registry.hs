-- | This merges all formula functions into one great registry.
-- 
-- This is necessary to have everything available in the implementation
-- of the tree evaluator.
-- 
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.


module FormulaEngine.Registry
(
 resolve
)

where

import qualified Data.Map as Map
import qualified Data.Plain as P

import qualified FormulaEngine.Functions.Numerics as NumFuncs
import qualified FormulaEngine.Functions.StringFuncs as StringFuncs
-- add more...

-- | There should be a more generic way to do this. Map containing name mapping
-- from a function name to its implementation. There should be a way to find
-- out by browsing the function files.
registry :: Map.Map String P.PlainFunction
registry = Map.fromList [("add", NumFuncs.add), ("conc", StringFuncs.conc)]

-- | Provides a default function returning error
defaultFunction :: String -- ^ Name of the (unmapped) function searched
                -> P.PlainFunction -- ^ Return value - function returning "function not found" error
defaultFunction name _ = P.PlError $ "Function " ++ name ++ "not found in registry"

-- | Resolve a function name into a function operating in a tree
resolve :: String -- ^ Name of the function
        -> P.PlainFunction -- ^ The function found in the registry
resolve name = case Map.lookup name registry of
                 Just func -> func
                 otherwise -> defaultFunction name
