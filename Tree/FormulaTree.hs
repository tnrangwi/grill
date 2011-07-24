-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.
module Tree.FormulaTree
(
 FormulaTree(..),
 TreeError
)

where

import qualified Data.Plain as P

-- | Type to mark error in tree
data TreeError = NamedError String -- ^ Named error - this should be used
               | UnspecifiedError -- ^ Unspecified error - not useful, better add more sepcific errors
                 deriving Show

-- | Compiled representation of a formula
data FormulaTree = Raw P.Plain -- ^ Node is plain result value
                 | Funcall ([P.Plain] -> P.Plain) [FormulaTree] -- ^ Node is a formula
                 | TreeError -- ^ Node is evaluated to an error


