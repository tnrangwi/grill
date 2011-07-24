-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.

module FormulaEngine.Parse
(
 compile
)

where

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P
import qualified FormulaEngine.Functions.Numerics as NumFuncs
import Text.ParserCombinators.Parsec

-- | Function to parse a string and return a compiled FormulaTree
compile :: String -> T.FormulaTree
compile = error "NYI"

-- | Formula parser to be used by Parsec
-- form :: Parsec.Parser T.FormulaTree
-- form = do
--  char '('
--  command <- word
--  -- ...
--  char ')'
--  return T.Funcall (func4word command) [forms]
