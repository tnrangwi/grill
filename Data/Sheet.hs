-- | This file defines data structure in a workheet.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Data.Sheet
    (
     RawSheet,
     emptyRawSheet,
     addCell,
     buildSheet,
     RawHeader,
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

-- | Create new empty sheet - use that for parsing a new sheet
emptyRawSheet :: RawSheet -- ^ Empty sheet created with defaults
emptyRawSheet = RSheet Map.empty

-- | Add a single cell to a raw sheet. May change and require certain conditions in the future.
addCell :: L.Address -- ^ Cell address
        -> T.FormulaTree -- ^ Tree to add into cell
        -> RawSheet -- ^ Sheet to update
        -> RawSheet -- ^ Updated sheet
addCell a t = RSheet . Map.insert a t . rSheet

-- | Create a sheet from (usually parsed) contents
buildSheet :: RawHeader -- ^ Sheet header
           -> [[T.FormulaTree]]
           -> Either String RawSheet
buildSheet header rows = -- FIXME: The limited data type currently does not put the header into the sheet datatype
    Right . RSheet . Map.fromList $ concatMap buildRow (zip [0..] rows)
        where
          buildRow (r, cs) = map (buildCell r) $ zip [0..] cs
          buildCell r (c, t) = (L.makeAddr r c, t)

-- | Return empty header to add properties later.
emptyRawHeader :: RawHeader
emptyRawHeader = RHeader Map.empty

-- | Add properties to header.
addHeaderProperties :: [(String, P.Plain)] -- ^ Property name / value
                    -> RawHeader -- ^ Old header
                    -> RawHeader -- ^ updated header
addHeaderProperties p = RHeader . Map.union (Map.fromList p) . rHeader
