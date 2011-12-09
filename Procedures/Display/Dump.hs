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

import qualified Data.List as List

import qualified Data.Plain as P
import qualified Tree.FormulaTree as T
import qualified Data.Sheet as S
import qualified Data.SheetLayout as L
import qualified FormulaEngine.Evaluate as E -- FIXME: Sheet calculation in this module?

-- | Dump objects in a more or less suitable way
class Dump a b where
    dump :: a -> b

instance Dump S.Sheet (IO ()) where
    dump sheet = let line = take 79 (List.repeat '=') ++ "\n"
                 in
                   putStr $ line ++ (dump sheet :: String) ++ line

-- FIXME: Replace by Data.Text
instance Dump S.Sheet String where
    dump sheet = concat [buildRow r | r <- [0..S.numRows sheet - 1] ]
        where
          maxCol = flip (-) 1 . flip S.numCols sheet
          buildRow r = show [show (E.calcCell sheet (L.makeAddr r c)) | c <- [0..maxCol r] ] ++ "\n"

{-
class Serialise a b where
    marshal :: a -> b

instance Serialise S.Sheet String where
    marshal sheet = concat [buildRow r | r <- [0..S.numRows - 1] ]
        where
          maxCol = flip (-) 1 . flip S.numCols sheet
          buildRow r = List.intersperse '\t' [(E.showCell sheet (L.makeAddr r c)) | c <- [0..maxCol r] ] ++ "\n"
-}
