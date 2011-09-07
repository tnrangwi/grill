-- | This file defines compile constants.
-- Do not use, use 'Version.Information' instead.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Version.Constants where

-- | Internals: String starting every grill sheet
grillPrefix :: String
grillPrefix = "tUrf"

-- | Internals: String ending signature of every grill steeht
grillSuffix :: String
grillSuffix = "frUt"

-- | Identifier to find sheet version.
sheetPrefix :: Char
sheetPrefix = 'V'

-- | Identifier to find required calculation engine.
calcEnginePrefix :: Char
calcEnginePrefix = 'C'

-- | Identifier to find checksum.
checksumPrefix :: Char
checksumPrefix = '\\'
