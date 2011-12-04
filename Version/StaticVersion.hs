-- | This file defines (editable) compile constants and currently is maintained manually.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

-- FIXME: Should use some template haskell and include data from a text file
-- FIXME: At least some repository information should be there

module Version.StaticVersion where

import Version.Types

grillMajor :: SmallVersion ; grillMinor :: SmallVersion ; grillMicro :: SmallVersion
formatMajor :: SmallVersion ; formatMinor :: SmallVersion ; formatMicro :: SmallVersion

grillMajor = 0
grillMinor = 0
grillMicro = 1

formatMajor = 0
formatMinor = 0
formatMicro = 1
