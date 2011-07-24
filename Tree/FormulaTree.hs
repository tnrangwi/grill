-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.
module Tree.FormulaTree
(
 FormulaTree(..),
 TreeError,
 eval
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

-- | Evaluate FormulaTree. Better place it in a different module?
eval :: FormulaTree -- ^ Compiled tree to evaluate
     -> P.Plain -- ^ Plain result value
eval (Raw v) = v
eval (Funcall f l) = f (map eval l)  --map f (eval l) 

-- eval TreeError = TreeErro

-- data Tree a f = Raw a -- ^ Plain item - number format or ++string format
--            | Funcall f [Tree a] -- ^ Another node: Function call, several node inputs

-- type FormulaTree = Tree Plain ([Plain] -> Plain)

-- type ParserTree = Tree 


