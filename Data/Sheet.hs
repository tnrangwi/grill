-- | This file defines data structure in a workheet, build and access functions.
-- 
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

--FIXME: Define a sheet, additional to RawSheet and use that one externally.
--FIXME: Interate sheet header data, maxrow and maxcol better in sheet.

module Data.Sheet
    (
     -- * Data types
     RawSheet,
     RawHeader,
     -- * Sheet functions
     emptyRawSheet,
     buildSheet,
     addCell,
     getCell,
     maxRow,
     maxCol,
     -- * Header functions
     emptyRawHeader,
     addHeaderProperties
    )

where

import qualified Data.Plain as P
import qualified Data.Map as Map
import qualified Tree.FormulaTree as T
import qualified Data.SheetLayout as L

-- | (Hidden) type for raw sheet: Just a map. May change in the future.
-- Most likely will become some well suited functional data structure (tree?).
newtype RawSheet = RSheet { rSheet :: (Map.Map L.Address T.FormulaTree) }

-- | Type for sheet header. Just a properties map. May cange in the future.
newtype RawHeader = RHeader { rHeader :: (Map.Map String P.Plain) }

------------------
-- Sheet functions
------------------

-- | Create new empty sheet - use that for parsing a new sheet
emptyRawSheet :: RawSheet -- ^ Empty sheet created with defaults
emptyRawSheet = RSheet Map.empty


-- | Create a sheet from (usually parsed) contents
buildSheet :: RawHeader -- ^ Sheet header
           -> [[T.FormulaTree]] -- ^ List of rows, rows itself lists of trees.
           -> Either String RawSheet -- ^ Error message or sheet structure.
buildSheet header rows =
    Right . RSheet . Map.fromList $
              (L.makeAddr (-1) (-1), len rows) : concatMap buildRow (zip [0..] rows)
        where
          len = T.Raw . P.PlInt . flip (-) 1 . length
          buildRow (r, cs) = (L.makeAddr r (-1), len cs) : map (buildCell r) (zip [0..] cs)
          buildCell r (c, t) = (L.makeAddr r c, t)


-- | Add a single cell to a raw sheet. May change and require certain conditions in the future.
addCell :: L.Address -- ^ Cell address
        -> T.FormulaTree -- ^ Tree to add into cell
        -> RawSheet -- ^ Sheet to update
        -> RawSheet -- ^ Updated sheet
addCell a t = RSheet . Map.insert a t . rSheet


-- | Get content of single cell
getCell :: RawSheet -- ^ The sheet.
        -> L.Address -- ^ Cell address.
        -> T.FormulaTree
getCell s a = Map.findWithDefault (T.Raw P.PlEmpty) a $ rSheet s


-- | Retrieve number of rows in sheet
maxRow :: RawSheet -- ^ The sheet.
       -> Int -- ^ Max row or -1.
maxRow s = case Map.lookup (L.makeAddr (-1) (-1)) $ rSheet s of
             Just (T.Raw (P.PlInt i)) -> i
             otherwise -> -1

-- | Retrieve maximum column in row
maxCol :: RawSheet -- ^ The sheet.
       -> Int -- ^ Row to search
       -> Int -- ^ Max column in row or -1
maxCol s r = case Map.lookup (L.makeAddr r (-1)) $ rSheet s of
               Just (T.Raw (P.PlInt i)) -> i
               otherwise -> -1


--------------------
-- Header functions.
--------------------

-- | Return empty header to add properties later.
emptyRawHeader :: RawHeader
emptyRawHeader = RHeader Map.empty

-- | Add properties to header.
addHeaderProperties :: [(String, P.Plain)] -- ^ Property name / value
                    -> RawHeader -- ^ Old header
                    -> RawHeader -- ^ updated header
addHeaderProperties p = RHeader . Map.union (Map.fromList p) . rHeader

