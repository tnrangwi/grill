-- | This file defines data structure in a workheet, build and access functions.
-- 
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.


module Data.Sheet
    (
     -- * Data types
     Sheet
    , Header
     -- * Sheet functions
    , createSheet
    , emptySheet
    , buildSheet
    , changeCell
    , getCell
    , numRows
    , numCols
     -- * Header functions
    , createHeader
    , requestHeader
    , addHeaderProperties
    -- * Miscellaneous functions
    , defaultName
    )

where

import qualified Data.Plain as P
import qualified Data.Map as Map
import qualified Tree.FormulaTree as T
import qualified Data.SheetLayout as L

import Version.Types (SmallVersion)
import qualified Version.Information as V

-- | Implementation type.
-- This type must not be exported. It exists here only for shortcuts in implementation.
data RawSheet = RawSheet {
      sCells :: Map.Map L.Address T.FormulaTree -- ^ Map of all cells
      , sNrows :: L.Coord -- ^ Max row that exists in the sheet. Empty rows are possible.
      , sNcols :: Map.Map L.Coord L.Coord -- ^ Max column for corresponding row. Empty cells are possible before.
      , sHeaderInfo :: RawHeader -- ^ Header information
    }

-- | (Hidden) type for raw sheet. May change in the future.
newtype Sheet = RSheet { rSheet :: RawSheet }

data RawHeader = RawHeader {
      hFormatVersion :: (SmallVersion, SmallVersion, SmallVersion) -- ^ Major, minor and patch level of sheet format.
                                                                   -- This is two numbers too much.
    , hCalcVersion :: (SmallVersion, SmallVersion, SmallVersion) -- ^ Major, minor and patch level of calculator lib.
    , hChecksum :: String -- ^ We do not need it now, reserved
    , hName :: String -- ^ The name of the sheet - there should be one for every sheet.
    , hFileName :: String  -- ^ File name of the sheet - if present. This only makes sense as long there is only one sheet.
    , hExtended :: Map.Map String P.Plain -- ^ Further custom attributes stored in a map.
    }

-- | Type for sheet header. Just a properties map. May change in the future.
newtype Header = RHeader { rHeader :: RawHeader }

------------------
-- Sheet functions
------------------
-- | Create a new sheet. Name is mandatory.
createSheet :: String -- ^ Name of this sheet.
           -> Sheet -- ^ Empty sheet created with defaults
createSheet name = RSheet RawSheet {
                    sCells = Map.empty
                  , sNrows = 0
                  , sNcols = Map.empty
                  , sHeaderInfo = rHeader $ createHeader name
                  }


-- | Create new empty sheet.
emptySheet :: Sheet -- ^ The default sheet returned.
emptySheet = createSheet defaultName

-- | Create a sheet from (usually parsed) contents
buildSheet :: Header -- ^ Sheet header
           -> [[T.FormulaTree]] -- ^ List of rows, rows itself lists of trees.
           -> Sheet -- ^ Sheet structure.
buildSheet header rows = RSheet RawSheet {
                           sCells = Map.fromList $ concatMap buildRow annotatedRows
                         , sNrows = fromIntegral . length $ rows
                         , sNcols = Map.fromList $ map (\(c, r) -> (c, fromIntegral $ length r)) annotatedRows
                         , sHeaderInfo = rHeader header
                         }
    where
      buildRow (r, cs) = map (buildCell r) (zip annotations cs)
      buildCell r (c, t) = (L.makeAddr r c, t)
      annotatedRows = zip annotations rows
      annotations = [0 :: L.Coord ..]

-- | Add / change a single cell to a raw sheet. May change and require certain conditions in the future.
changeCell :: L.Address -- ^ Cell address
           -> T.FormulaTree -- ^ Tree to add into cell
           -> Sheet -- ^ Sheet to update
           -> Sheet -- ^ Updated sheet
-- FIXME: Set max updater for row and / or column to id, if nothing to do.
-- Insert is quite more expensive then id and id will be sufficint in most cases.
changeCell a t s = RSheet $ rawsheet { sCells = Map.insert a t (sCells rawsheet)
                                     , sNrows = max (r + 1) (sNrows rawsheet)
                                     , sNcols = Map.insert r (max (c + 1) $ Map.findWithDefault 0 r nCols) nCols
                                     }
    where rawsheet = rSheet s
          nCols = sNcols rawsheet
          r = L.row a
          c = L.col a

-- | Get content of single cell
getCell :: Sheet -- ^ The sheet.
        -> L.Address -- ^ Cell address.
        -> T.FormulaTree
getCell s a = Map.findWithDefault (T.Raw P.PlEmpty) a . sCells . rSheet $ s

-- | Number of rows in the sheet
numRows :: Sheet -- ^ The sheet
        -> L.Coord -- ^ Number of rows
numRows = sNrows . rSheet

-- | Number of columns of a particular row.
numCols :: L.Coord -- ^ The row of interest.
        -> Sheet -- ^ The sheet.
        -> L.Coord -- ^ Number of columns in the row.
numCols n = Map.findWithDefault 0 n . sNcols . rSheet


--------------------
-- Header functions.
--------------------

-- | Create header with default settings. A name is mandatory.
createHeader :: String -- ^ Mandatory name of the sheet
             -> Header -- ^ Default header returned
createHeader n = RHeader RawHeader {
                   hFormatVersion = V.formatVersion
                 , hCalcVersion = V.grillVersion
                 , hChecksum = ""
                 , hName = n
                 , hFileName = ""
                 , hExtended = Map.empty
                 }

-- | Request for a valid header for given versions.
requestHeader :: (SmallVersion, SmallVersion, SmallVersion) -- ^ Format of this sheet
              -> (SmallVersion, SmallVersion, SmallVersion) -- ^ Level of calculation engine
              -> String -- ^ Checksum -- FIXME: use a typedef for that
              -> String -- ^ Sheet name
              -> Either String Header -- ^ Error message or header
requestHeader format calc _ name = -- checksum not needed up to now
    case V.checkFormat format of
      Just err -> Left err
      Nothing -> case V.checkGrill calc of
                   Just err -> Left err
                   Nothing -> Right $ RHeader RawHeader {
                                          hFormatVersion = format
                                        , hCalcVersion = calc
                                        , hChecksum = ""
                                        , hName = name
                                        , hFileName = ""
                                        , hExtended = Map.empty
                                        }

-- | Add properties to header.
addHeaderProperties :: [(String, P.Plain)] -- ^ Property name / value
                    -> Header -- ^ Old header
                    -> Header -- ^ updated header
addHeaderProperties p h = RHeader $ rawheader { hExtended = Map.fromList p `Map.union` extended }
    where rawheader = rHeader h
          extended = hExtended rawheader

----------------------------
-- Stubs
----------------------------

-- | Return the default name for a newly created sheet. This may change if there are more than one
-- sheet bundled in a workbook as in that case there must be a counter to give any sheet a different name.
defaultName :: String
defaultName = "Unnamed"
