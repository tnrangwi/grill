-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.
module FormulaEngine.Evaluate
(
 eval
)

where

import qualified Data.Plain as P
import qualified Tree.FormulaTree as T


-- | Evaluate FormulaTree.
eval :: T.FormulaTree -- ^ Compiled tree to evaluate
     -> P.Plain -- ^ Plain result value
eval (T.Raw v) = v
eval (T.TreeError e) = P.PlError (show e)
eval (T.Funcall f l) = f (map eval l)
