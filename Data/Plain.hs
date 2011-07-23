-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.
module Plain
(
 GenericResult,
 Plain(..)
)

where

-- | Generic object - this is to hold results that cannot be shown (like the result of an external library.
-- | I currently don't know how this should be implemented but I am sure we will need that even for
-- | internal use.

data GenericResult = GResError String -- ^ Error returned, information in string
                   | GResXYZ Int -- ^ Example to test implementation

-- | Plain data type containing some plain value or a composite result that should not be evaluated further
data Plain = PlEmtpy -- ^ Empty item
           | PlInt Int -- ^ Int value
           | PlFloat Float -- ^ Float value
           | PlString String -- ^ String value
           | PlComposite GenericResult

