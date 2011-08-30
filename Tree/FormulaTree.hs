-- | This file defines 'FormulaTree', the type used for modeling the calculation trees.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Tree.FormulaTree
(
 FormulaTree(..),
 TreeError(..)
)

where

import qualified Data.Plain as P
import qualified Data.SheetLayout as L

-- | Type to mark error in tree
data TreeError = NamedError String -- ^ Named error - this should be used
               | CircularReference -- ^ Circular reference in tree.
               | UnspecifiedError -- ^ Unspecified error - not useful, better add more specific errors
                 deriving Show

-- | Compiled representation of a formula
data FormulaTree = Raw P.Plain -- ^ Node is plain result value
                 | Reference L.Address
                 | Funcall P.PlainFunction [FormulaTree] -- ^ Node is a formula
                 | TreeError TreeError -- ^ Node is evaluated to an error
                                       -- This makes sense for errors affecting the tree like circular references
                                       -- in contrast to plain value errors like div/0.
