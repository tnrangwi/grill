-- | This file dumps any kind of structures.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Procedures.Serialise.Dump
(
 Dump(..)
)

where

import qualified Data.List as List

import qualified Data.Plain as P
import qualified Tree.FormulaTree as T
import qualified Data.Sheet as S
import qualified Data.SheetLayout as L
import qualified FormulaEngine.Evaluate as E

-- | Dump objects in a more or less suitable way
class Dump a b where
    dump :: a -> b
    eval :: a -> b

instance Dump S.Sheet (IO ()) where

    dump sheet = putStr (dump sheet :: String)

    eval sheet = let line = replicate 79 '=' ++ "\n"
                 in
                   putStr $ line ++ (eval sheet :: String) ++ line

-- FIXME: Replace by Data.Text
instance Dump S.Sheet String where
    dump sheet = concat [buildRow r | r <- buildList (S.numRows sheet) ]
        where
          buildList n = if n > 0 then [0..n - 1] else []
          nCols = flip S.numCols sheet
          buildRow r = concat (List.intersperse "\t" 
                                       [E.showTree (S.getCell sheet (L.makeAddr r c)) | c <- buildList (nCols r )])
                       ++ "\n"

    eval sheet = concat [buildRow r | r <- buildList (S.numRows sheet) ]
        where
          buildList n = if n > 0 then [0..n - 1] else []
          nCols = flip S.numCols sheet
          buildRow r = show [show (E.calcCell sheet (L.makeAddr r c)) | c <- buildList (nCols r) ] ++ "\n"
