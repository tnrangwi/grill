-- | This file implements evaluation of tree structure used for the formulas.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module FormulaEngine.Evaluate
(
 eval
)

where

import qualified Data.Plain as P
import qualified Tree.FormulaTree as T


-- FIXME: Eval needs to be enhanced for sheet evaluation with references

-- | Evaluate FormulaTree.
eval :: T.FormulaTree -- ^ Compiled tree to evaluate
     -> P.Plain -- ^ Plain result value
eval (T.Raw v) = v
eval (T.TreeError e) = P.PlError $ show e
eval (T.Funcall f l) = f $ map eval l
eval (T.Reference addr) = P.PlError $ "Cannot evaluate address without a sheet - referenced:" ++ (show addr) ++ "."
