-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.

module FormulaEngine.Parse
(
 parse
)

where

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P
import qualified FormulaEngine.Functions.Numerics as NumFuncs

-- | Function to parse a string and return a compiled FormulaTree
parse :: String -> T.FormulaTree
parse = error "NYI"