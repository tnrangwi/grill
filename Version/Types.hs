-- | This file defines typedefs for all version informationsn.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Version.Types
(
 SmallVersion
)

where

import qualified Data.Word

type SmallVersion = Data.Word.Word8
