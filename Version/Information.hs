-- | This file provides and helps extract version information.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Version.Information
    (
     -- * Compile time and forever constants
     module Constants,
     module StaticVersion,
     -- * Version information
     versionString,
     formatString,
     -- * Parser to parse version from a string - only used by parsers
     parseMagicBytes,
     parseFormat,
     parseVersion,
     parseChecksum
    )

where

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.Printf as Printf

import qualified Version.Constants as Constants
import qualified Version.StaticVersion as StaticVersion

-- | Version coded as string
versionString :: String
versionString = Printf.printf "%02d.%02d.%02d" StaticVersion.grillMajor StaticVersion.grillMinor StaticVersion.grillMicro

-- | Maximum sheet format grill can handle
formatString :: String -- FIXME: These should be different
formatString = versionString

-- | Parse magic bytes at the beginning of every grill sheet
parseMagicBytes :: Parsec.Parser ()
parseMagicBytes = Parsec.string Constants.grillPrefix >> return ()

-- | Help stub to parse one of the versions in the sheet header
parseVersionString :: Char -- ^ Identifier
                   -> String -- ^ String for fail messages
                   -> Parsec.Parser String -- ^ Parsec data type for string parsers
parseVersionString prefix msg = do
  Parsec.char prefix
  major <- Parsec.many1 Parsec.digit
  Parsec.char '.'
  minor <- Parsec.many1 Parsec.digit
  Parsec.char '.'
  micro <- Parsec.many1 Parsec.digit
  let res = major ++ "." ++ minor ++ "." ++ micro
  if length res == 8
      then
          return res
      else
          fail $ "Invalid version string:" ++ res ++ " for " ++ msg ++ "."

-- | Parse sheet version in header.
parseFormat :: Parsec.Parser String
parseFormat = parseVersionString Constants.sheetPrefix "sheet"

-- | Parse calc engine version in header.
parseVersion :: Parsec.Parser String
parseVersion = parseVersionString Constants.calcEnginePrefix "calc engine"

-- | Parse checksum in sheet header. Not yet implemented, always gives empty string currently.
parseChecksum :: Parsec.Parser String
parseChecksum = do
  Parsec.char Constants.checksumPrefix
  Parsec.spaces
  Parsec.newline
  return ""