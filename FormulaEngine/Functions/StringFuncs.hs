-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.

module FormulaEngine.Functions.StringFuncs
(
 conc
)

where

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P

-- Fixme: add concatenation for integers, floats
-- | Function to concatenate plain strings. FIXME: Add concatenation for int and float.
conc :: [P.Plain] -> P.Plain
conc = foldl step P.PlEmpty
    where step P.PlEmpty p@(P.PlString _) = p
          step (P.PlString v1) (P.PlString v2) = P.PlString $ v1 ++ v2
          step p@(P.PlError _) _ = p
          step _ _ = P.PlError "Invalid data type in conc"