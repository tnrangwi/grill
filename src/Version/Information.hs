-- | This file provides and helps extract version information.
--
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.

module Version.Information
    (
     -- * Compile time and forever constants
     module Version.Constants,
     module Version.StaticVersion,
     -- * Version information
     versionString,
     formatString,
     grillVersion,
     formatVersion,
     -- * Validate version
     checkGrill,
     checkFormat,
     -- * Parser to parse version from a string - only used by parsers
     parseMagicBytes,
     parseFormat,
     parseVersion,
     parseChecksum,
     parseEndOfHeader
    )

where

import qualified Control.Monad as Monad

import qualified Text.ParserCombinators.Parsec as Parsec
import Text.Printf as Printf

import Version.Constants -- needs to be imported unqualified or re-export will not work
import Version.StaticVersion -- same here
import Version.Types (SmallVersion)

-- | Version coded as string
versionString :: String
versionString = Printf.printf "%02d.%02d.%02d"
                (fromIntegral Version.StaticVersion.grillMajor :: Int)
                (fromIntegral Version.StaticVersion.grillMinor :: Int)
                (fromIntegral Version.StaticVersion.grillMicro :: Int)

-- | Grill's calculation version.
grillVersion :: (SmallVersion, SmallVersion, SmallVersion)
grillVersion = (Version.StaticVersion.grillMajor, Version.StaticVersion.grillMinor, Version.StaticVersion.grillMicro)

-- | Sheet format version.
formatVersion :: (SmallVersion, SmallVersion, SmallVersion)
formatVersion = (Version.StaticVersion.formatMajor, Version.StaticVersion.formatMinor, Version.StaticVersion.formatMicro)

-- | Maximum sheet format grill can handle
formatString :: String
formatString = Printf.printf "%02d.%02d.%02d"
                (fromIntegral Version.StaticVersion.formatMajor  :: Int)
                (fromIntegral Version.StaticVersion.formatMinor :: Int)
                (fromIntegral Version.StaticVersion.formatMicro :: Int)

-- | Parse magic bytes at the beginning of every grill sheet
parseMagicBytes :: Parsec.Parser ()
parseMagicBytes = Parsec.string Version.Constants.grillPrefix >> return ()

-- | Help stub to parse one of the versions in the sheet header
parseVersionString :: Char -- ^ Identifier
                   -> String -- ^ String for fail messages
                   -> Parsec.Parser (SmallVersion, SmallVersion, SmallVersion) -- ^ Parsec data type for string parsers
parseVersionString prefix msg = do
  _ <- Parsec.char prefix
  sMajor <- Parsec.many1 Parsec.digit
  _ <- Parsec.char '.'
  sMinor <- Parsec.many1 Parsec.digit
  _ <- Parsec.char '.'
  sMicro <- Parsec.many1 Parsec.digit
  Monad.when (length sMajor /= 2 || length sMinor /= 2 || length sMicro /= 2)
           (fail $ "Invalid version string:" ++ sMajor ++ "." ++ sMinor ++ "." ++ sMicro ++ " for " ++ msg ++ ".")
  return (read sMajor :: SmallVersion, read sMinor, read sMicro)

-- | Parse sheet version in header.
parseFormat :: Parsec.Parser (SmallVersion, SmallVersion, SmallVersion)
parseFormat = parseVersionString Version.Constants.sheetPrefix "sheet"

-- | Parse calc engine version in header.
parseVersion :: Parsec.Parser (SmallVersion, SmallVersion, SmallVersion)
parseVersion = parseVersionString Version.Constants.calcEnginePrefix "calc engine"

-- | Parse checksum in sheet header. Not yet implemented, always gives empty string currently.
parseChecksum :: Parsec.Parser String
parseChecksum = do
  _ <- Parsec.char Version.Constants.checksumPrefix
  c <- Parsec.many $ Parsec.oneOf "ABCDEF0123456789"
  return c -- FIXME: The whole function should be possible without do and without return

-- | Parse header terminator.
parseEndOfHeader :: Parsec.Parser ()
parseEndOfHeader = Parsec.string Version.Constants.grillSuffix >> return ()

-- | Check version help stub.
checkVersion :: (SmallVersion, SmallVersion, SmallVersion) -- ^ The version to check.
             -> SmallVersion  -- ^ The major version supported
             -> String -- ^ String for error message construction
             -> Maybe String
checkVersion (m, _, _) c s = if m > c then
                                 Just $ "Your grill supports " ++ s ++ " version " ++ show c
                                          ++ ", but sheet requires version " ++ show m ++ "."
                             else
                                 Nothing

-- | Check a given version if it is supported by the current calculation.
checkGrill :: (SmallVersion, SmallVersion, SmallVersion)
           -> Maybe String
checkGrill v = checkVersion v Version.StaticVersion.grillMajor "calculation engine"

-- | Check a given version if it is supported by the current sheet formatter.
checkFormat :: (SmallVersion, SmallVersion, SmallVersion)
            -> Maybe String
checkFormat v = checkVersion v Version.StaticVersion.formatMajor "sheet format"
