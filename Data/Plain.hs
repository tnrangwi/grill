-- | This file defines 'Plain', the base type used for the calculation trees.
--
--   Author: Thorsten Rangwich. See file LICENSE for details.

module Data.Plain
(
 -- * Base Data Types supported in Tree
 Plain(..),
 -- * Type Classes for Base Data Types
 Convert(..),
 -- * Tree Function Interface
 PlainFunction,
 -- * Experimental
 GenericResult -- export more of it?
)

where

-- | Generic object - this is to hold results that cannot be shown (like the result of an external library).
-- I currently don\'t know how this should be implemented but I am sure we will need that even for
-- internal use.
-- FIXME: Use a special type class instead with methods for display in cell,
-- possibly serialisation or whatever will be needed.
data GenericResult = GResError String -- ^ Error returned, information in string
                   | GResXYZ Int -- ^ Example to test implementation
                     deriving Show

-- | Plain data type containing some plain value or a composite result that should not be evaluated further.
data Plain = PlEmpty -- ^ Empty item
           | PlInt Int -- ^ Int value
           | PlFloat Float -- ^ Float value
           | PlString String -- ^ String value
           | PlError String -- ^ Error occured - FIXME: Better to packlage whole plain in Either instead?
           | PlComposite GenericResult
             deriving Show

-- | Function type for every tree function. Input takes a list of plains and returns one Plain value.
type PlainFunction = ([Plain] -> Plain)

-- | Class to convert Plain into raw data type and vice versa.
class Convert a where
    -- | Convert Plain data type into its raw data type. Has to raise error in default implementation.
    get :: Plain -- ^ Input Plain
        -> a -- ^ Raw data type for return

    get _ = error "Unexpected type - add instance declaration to unpack 'Plain' type"

    -- | Convert arbitrary data type into a packaged Plain. Default implementation returns PlError.
    toPlain :: a -- ^ Input arbitrary data type.
            -> Plain -- ^ Output - packaged in Plain.
    toPlain _ = PlError "No 'Plain' implementation for this data type"


instance Convert Int where
    get (PlInt v) = v
    get _ = error "No plain integer value"

