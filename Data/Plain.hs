-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.
module Data.Plain
(
 GenericResult,
 Plain(..),
 Extract(..)
)

where

-- | Generic object - this is to hold results that cannot be shown (like the result of an external library.
-- | I currently don't know how this should be implemented but I am sure we will need that even for
-- | internal use.

data GenericResult = GResError String -- ^ Error returned, information in string
                   | GResXYZ Int -- ^ Example to test implementation
                     deriving Show

-- | Plain data type containing some plain value or a composite result that should not be evaluated further
data Plain = PlEmpty -- ^ Empty item
           | PlInt Int -- ^ Int value
           | PlFloat Float -- ^ Float value
           | PlString String -- ^ String value
           | PlError String -- ^ Error occured - FIXME: Better to packlage whole plain in Either instead?
           | PlComposite GenericResult
             deriving Show

class Extract a where
    get :: Plain -> a
    get _ = error "Unexpected type - add instance declaration to unpack 'Plain' type"

instance Extract Int where
    get (PlInt v) = v
    get _ = error "No plain integer value"