-- | This file defines data structure in a workheet.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Data.Sheet
(
 RawSheet,
 emptyRawSheet
)

where

import qualified Data.Plain as P
import qualified Data.Map as Map
import qualified Tree.FormulaTree as T
import qualified Data.SheetLayout as L

-- | Type for raw sheet: Just a map.
newtype RawSheet = RSheet (Map.Map L.Address T.FormulaTree)

-- | Create new empty sheet - use that for parsing a new sheet
emptyRawSheet :: RawSheet -- ^ Empty sheet created with defaults
emptyRawSheet = RSheet Map.empty

--addCell :: L.Address -> T.FormulaTree -> RawSheet -> RawSheet
--addCell a t s = Map.insert a t s
