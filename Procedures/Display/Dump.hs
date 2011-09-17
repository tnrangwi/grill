-- | This file dumps any kind of structures.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Procedures.Display.Dump
(
 Dump(..)
)

where

import qualified Data.Plain as P
import qualified Tree.FormulaTree as T
import qualified Data.Sheet as S
import qualified FormulaEngine.Evaluate as E -- FIXME: Sheet calculation in this module?

-- | Dump objects in a more or less suitable way
class Dump a b where
    dump :: a -> b

instance Dump S.RawSheet (IO ()) where
    dump sheet = print ( (dump sheet) :: String )

-- FIXME: Replace by Data.Text
instance Dump S.RawSheet String where
    dump sheet = "Aetsch! Sheet anzeigen gibt es noch nicht"
    