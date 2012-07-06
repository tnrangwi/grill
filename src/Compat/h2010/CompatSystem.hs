-- | This file defines some stuff to make it compatible to haskell 2010 instead of haskell 98 (both with ghc extensions)
--
--   Author: Thorsten Rangwich. See file <../LICENSE> for details.
module CompatSystem

(
 getProgName,
 getArgs
)

where

import System.Environment
