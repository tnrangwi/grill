-- | This file defines numerical functions available in formulas.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.
module FormulaEngine.Functions.Numerics
(
 add
)

where

import qualified Data.Plain as P

-- | Function to numerical add numbers.
-- FIXME: Add combinations for Int and Float.
add :: [P.Plain] -> P.Plain
add = foldl step P.PlEmpty
    where step P.PlEmpty p@(P.PlInt _) = p
          step P.PlEmpty p@(P.PlFloat _) = p
          step (P.PlInt v1) (P.PlInt v2) = P.PlInt $ v1 + v2
          step (P.PlFloat v1) (P.PlFloat v2) = P.PlFloat $ v1 + v2
          step p@(P.PlError _) _ = p
          step _ _ = P.PlError "Invalid data type or data type combination in add"
