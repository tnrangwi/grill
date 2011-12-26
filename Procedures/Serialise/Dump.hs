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


-- | Dump objects in a more or less suitable way. Either evaluated or raw.
class Dump a b where

    -- | Dump unevaluated.
    dump :: a -- ^ Arbitrary input type to dump.
         -> b -- ^ Arbitrary output type.

    -- | Dump evaluated.
    eval :: a -- ^ Arbitrary input type to calculate.
         -> b -- ^ Arbitrary outout type.

    -- | Pretty print evaluated dump.
    pprint :: a -- ^ Arbitrary input type for nice evaluated print.
           -> b -- ^ Arbitrary output type.
    pprint a = error "Pretty print not yet implemented"


instance Dump S.Sheet (IO ()) where

    dump sheet = putStr (dump sheet :: String)

    eval sheet = putStr (eval sheet :: String)


-- | Help type to do all dumps with one function. Put everything in one function to reduce duplicate code.
data DumpType = DumpRaw
              | DumpEvaluated
              | DumpPretty

-- | Only buildrow is different for every dump type.
dump' :: DumpType
      -> S.Sheet
      -> String
dump' t sheet = concat [buildRow r | r <- buildList (S.numRows sheet) ]
        where
          buildList n = if n > 0 then [0..n - 1] else []
          nCols = flip S.numCols sheet
          buildRow r = case t of
                         DumpRaw -> List.intercalate "\t" 
                                     [E.showTree (S.getCell sheet (L.makeAddr r c)) | c <- buildList (nCols r)]
                                    ++ "\n"
                         DumpEvaluated -> show [show (E.calcCell sheet (L.makeAddr r c)) | c <- buildList (nCols r)] ++ "\n"
                         _ -> error "Not yet implemented for this dump type"


instance Dump S.Sheet String where
    dump = dump' DumpRaw

    eval = dump' DumpEvaluated
