-- | This file implements evaluation of tree structure used for the formulas.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module FormulaEngine.Evaluate
(
 eval,
 calcCell
)

where

import qualified Data.Plain as P
import qualified Tree.FormulaTree as T
import qualified Data.Sheet as S
import qualified Data.SheetLayout as L

-- FIXME: Eval needs to be enhanced for sheet evaluation with references

-- | Evaluate FormulaTree.
eval :: T.FormulaTree -- ^ Compiled tree to evaluate
     -> P.Plain -- ^ Plain result value
eval (T.Raw v) = v
eval (T.TreeError e) = P.PlError $ show e
eval (T.Funcall f l) = let argv = map eval l
                           res = f argv
                       in
                         case res of
                           P.PlError _ -> findError res argv
                           otherwise -> res
eval (T.Reference addr) = P.PlError $ "Cannot evaluate address without a sheet - referenced:" ++ show addr ++ "."

-- | Check if error already in input or in result of function
findError :: P.Plain -- ^ Result error value
           -> [P.Plain]  -- ^ Input arguments
           -> P.Plain -- ^ Input argument error or result error
findError r argv = let errors = filter P.checkError argv
                   in
                     if length errors > 0 then
                         head errors -- FIXME: Build a summary message with all errors?
                     else
                         r

-- | Calculate one cell in a sheet and return Plain value.
calcCell :: S.RawSheet -- ^ The sheet to calculate a cell
         -> L.Address -- ^ The cell address we are interested in
         -> P.Plain -- ^ Plain value suitable for show
calcCell sheet addr = P.PlError "calcCell not yet implemented"
