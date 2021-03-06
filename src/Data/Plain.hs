-- | This file defines 'Plain', the base type used for the calculation trees.
--
--   Author: Thorsten Rangwich. See file <../LICENSE> for details.

{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Plain
(
 -- * Base Data Types supported in Tree
 Plain(..),
 -- * Type Classes for Base Data Types
 Convert(..),
 repr,
 -- * Tree Function Interface
 PlainFunction,
 checkError
)

where

{- FIXME: A Plain should be a plain. The trees could return some kind of unserialisable result, which is needed
  to be analysed further.

-- | Generic object - this is to hold results that cannot be shown (like the result of an external library).
-- I currently don\'t know how this should be implemented but I am sure we will need that even for
-- internal use.
-- FIXME: Use a special type class instead with methods for display in cell,
-- possibly serialisation or whatever will be needed.
-- data GenericResult = GResError String -- ^ Error returned, information in string
--                   | GResXYZ Int -- ^ Example to test implementation
--                     deriving Show
-}

-- | Plain data type containing some plain value or a composite result that should not be evaluated further.
data Plain = PlEmpty -- ^ Empty item
           | PlInt Int -- ^ Int value
           | PlFloat Float -- ^ Float value
           | PlString String -- ^ String value
           | PlError String -- ^ Error occured
             deriving Show

-- | Check Plain for error
checkError :: Plain -- ^ Plain value to check for error
           -> Bool -- ^ Return value - error contained in Plain?
checkError (PlError _) = True
checkError _ = False

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
    get PlEmpty = 0
    get _ = error "No plain integer value"

instance Convert Float where
    get (PlFloat v) = v
    get PlEmpty = 0
    get _ = error "No plain float value"

instance Convert String where
    get (PlString v) = v
    get PlEmpty =  []
    get _ = error "No plain string value"


-- | Get a string representation
repr :: Plain -- ^ Plain value
     -> String -- ^ String representation
repr PlEmpty = ""
repr (PlInt a) = show a
repr (PlFloat a) = show a
repr (PlString a) = "\"" ++ 
                  concatMap (\c -> if c `elem` "\\\"" then ['\\', c] else [c]) a
                  ++ "\""
repr (PlError m) = '!' : show m -- FIXME: Escape xyz
