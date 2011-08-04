-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.
module Data.Plain
(
 GenericResult,
 Plain(..),
 Convert(..),
 PlainFunction
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

type PlainFunction = ([Plain] -> Plain)

-- | Convert Plain into raw data type and vice versa
class Convert a where
    -- | Convert Plain data type into its raw data type
    get :: Plain -- ^ Input Plain
        -> a -- ^ Raw data type for return
    -- | Standard implementation - raise error
    get _ = error "Unexpected type - add instance declaration to unpack 'Plain' type"
    toPlain :: a -> Plain
    toPlain _ = PlError "No 'Plain' implementation for this data type"


instance Convert Int where
    get (PlInt v) = v
    get _ = error "No plain integer value"

