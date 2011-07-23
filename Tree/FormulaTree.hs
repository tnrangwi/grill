-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.
module FormulaTree
(
 FormulaTree(..),
 TreeError
)

where

import qualified Data.Plain as Plain

-- | Type to mark error in tree
data TreeError = NamedError String -- ^ Named error - this should be used
               | UnspecifiedError -- ^ Unspecified error - not useful, better add more sepcific errors

data FormulaTree = Raw Plain -- ^ Plain result value
                 | Funcall ([Plain] -> Plain) [FormulaTree] -- ^ Input is a formula
                 | TreeError



-- data Tree a f = Raw a -- ^ Plain item - number format or ++string format
--            | Funcall f [Tree a] -- ^ Another node: Function call, several node inputs

-- type FormulaTree = Tree Plain ([Plain] -> Plain)

-- type ParserTree = Tree 


