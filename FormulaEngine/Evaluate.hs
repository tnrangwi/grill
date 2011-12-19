-- | This file implements evaluation of tree structure used for the formulas.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module FormulaEngine.Evaluate
(
 calcTree,
 calcCell,
 showTree
)

where

import qualified Data.List as List

import qualified Data.Plain as P
import qualified Tree.FormulaTree as T
import qualified Data.Sheet as S
import qualified Data.SheetLayout as L


-- | Type to encapsulate evaluates with and without sheet references.
data EvalRef = NoRef -- ^ Eval formula tree without sheet references.
             | SheetRef S.Sheet -- ^ Eval with sheet
             | FullRef S.Sheet [L.Address] -- ^ Full eval with sheet reference and list of used cells.
                                               -- Params are sheet reference and a list of
                                               -- previously evaluated cells to check for circular references.

-- | Evaluate FormulaTree.
eval :: T.FormulaTree -- ^ Compiled tree to evaluate
     -> EvalRef -- ^ The sheet, if other cells are referenced
     -> P.Plain -- ^ Plain result value
eval (T.Raw v) _ = v
eval (T.TreeError e) _ = P.PlError $ show e
eval (T.Funcall f l) r = let argv = map (`eval` r) l
                             func = T.funcCall f
                             res = func argv
                         in
                           case res of
                             P.PlError _ -> findError res argv
                             otherwise -> res
eval (T.Reference a) (FullRef s xs) = if a `elem` xs then
                                          P.PlError $ "Circular reference. Twice referenced:" ++ show a
                                      else
                                          eval (S.getCell s a) (FullRef s (a:xs))
eval r@(T.Reference a) (SheetRef s) = eval r (FullRef s [])
eval (T.Reference a) (NoRef) = P.PlError $ "Cannot evaluate address without a sheet - referenced:" ++ show a ++ "."


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
calcCell :: S.Sheet -- ^ The sheet to calculate a cell
         -> L.Address -- ^ The cell address we are interested in
         -> P.Plain -- ^ Plain value suitable for show
calcCell  sheet addr = eval (T.Reference addr) (SheetRef sheet)

-- | Calculate a tree - without a sheet reference. Useful for tests only.
calcTree :: T.FormulaTree -- ^ Tree for evaluation.
         -> P.Plain -- ^ Return value
calcTree t = eval t NoRef


-- | Show serialised format (edit format) of a cell (i.e. tree)
showTree :: T.FormulaTree -- ^ Tree to display
         -> String
showTree (T.Raw v) = P.repr v
showTree (T.TreeError e) = error "You should never be here: Erranous tree cannot be serialised for saving"
showTree (T.Funcall f l) = "(" ++ T.funcName f ++ " " ++ concat (List.intersperse " " (map showTree l)) ++ ")"
showTree (T.Reference a) = '\'' : L.showAddress a
