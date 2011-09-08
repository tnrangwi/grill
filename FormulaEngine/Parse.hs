-- | This file implements parsing from strings into formula trees.
--
-- | Author: Thorsten Rangwich. See file <../LICENSE> for details.

module FormulaEngine.Parse
(
 compileTree,
 compileSheet
)

where

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P
import qualified FormulaEngine.Functions.Numerics as NumFuncs
import qualified FormulaEngine.Registry as Reg
import qualified Data.SheetLayout as SheetLayout
import qualified Data.Sheet as Sheet
import qualified Version.Information as Version
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>), (<?>))

import qualified Control.Monad as M


-- | Function to parse a string and return a compiled FormulaTree
compileTree :: String -> T.FormulaTree
compileTree s = case Parsec.parse term "" s of
                  Left err -> T.TreeError . T.NamedError . show $ err
                  Right v -> v

-- FIXME: There should be a function using a handle instead of a string. Creating the string is very expensive!

-- | Function to parse a whole sheet.
compileSheet :: String -> Either String Sheet.RawSheet -- FIXME: Return something else on error?
compileSheet s = case Parsec.parse sheet "" s of
                   Left err -> Left $ show err
                   Right v -> Right v

sheet :: Parsec.Parser Sheet.RawSheet
sheet = do
          header <- sheetHeader
          fail "Not fully implemented"
          -- Control.Monad.when (Sheet.invalid header) ...
          

-- | Parse sheet header
sheetHeader :: Parsec.Parser Sheet.RawHeader
sheetHeader = do -- parse sheet version 1
                Version.parseMagicBytes
                version <- Version.parseSheetVersion
                calcVersion <- Version.parseCalcVersion
                Version.parseChecksum
                let v = Version.versionString
                    -- FIXME: Should be different versions, test only
                M.when (version > v || version > v) (fail "Your grill version is too old to open this")
                return Sheet.emptyRawHeader -- FIXME: Add contents!
            <?> "expecting sheet header"

-- | Formula parser to be used by compile
term :: Parsec.Parser T.FormulaTree
term = do -- parse function call containing more terms
         (command, args) <- parseFunction
         Parsec.spaces
         return $ T.Funcall (Reg.resolve command) args
     <|> -- parse escaped string
       do
         word <- parseString
         Parsec.spaces
         return . T.Raw . P.PlString $ word
     <|> -- parse reference to another tree
       do
         addr <- parseReference
         Parsec.spaces
         return $ T.Reference addr
     <|> -- This is just an example how to add a negative matching or a cell reference. Negative matching
         -- easily brings up funny problems and should not be done. FIXME: Should be deleted
       do
         -- The right parantheses is necessary, otherwise the plain expressions in a paranthesis expression
         -- will not terminate properly
         c <- Parsec.noneOf "0123456789+-)"
         fail $ "Invalid character:" ++ [c] ++ ". References or special treatment not yet implemented"
     <|> -- parse a number as float or integer. This has to be the last one to parse.
       do
         number <- parseNumber
         Parsec.spaces
         return . T.Raw $ number
     <?> "function term, number, string or reference"

-- | Parse a function call
parseFunction :: Parsec.Parser (String, [T.FormulaTree])
parseFunction = do
  Parsec.char '('
  Parsec.spaces
  command <- Parsec.many1 Parsec.letter
  Parsec.spaces
  args <- Parsec.many term
  Parsec.char ')'
  return (command, args)


-- | Parse String, double quote escapes a double quote. Take rest as is.
_escapedChar :: Parsec.Parser Char
_escapedChar = do
                 -- FIXME: try is quite expensive. It is possible without it, however, I didn't manage.
                 Parsec.try $ Parsec.string "\"\""
                 return '"'
             <|>
               Parsec.noneOf "\""
             <?> "It is impossible to see this error message unless code above is changed!"


-- | Parse a string.
parseString :: Parsec.Parser String
parseString = do
  Parsec.char '"'
  word <- Parsec.many _escapedChar
  Parsec.char '"'
  return word


-- | Parse a number, return packaged in Plain (either Float or Int)
parseNumber :: Parsec.Parser P.Plain
parseNumber = do
  sign <- Parsec.option '+' $ Parsec.oneOf "+-"
  prefix <- Parsec.many1 Parsec.digit -- could do without a digit (like .5) as well with special treatment afterwards
  comma <- Parsec.option 'X' $ Parsec.char '.'
  if comma == 'X' --no comma
    then
        return . P.PlInt $ getSign sign * read prefix
    else
        do
          suffix <- Parsec.many1 Parsec.digit -- could do without a digit (like 1.), see above
          return . P.PlFloat $ getSign sign * read (prefix ++ "." ++ suffix)


-- | Parse a reference to another tree, addressed as external cell address
parseReference :: Parsec.Parser SheetLayout.Address
parseReference = do
  Parsec.char '\''
  stringRow <- Parsec.many1 Parsec.digit
  Parsec.char ':'
  stringCol <- Parsec.many1 Parsec.digit
  let row = read stringRow
  let col = read stringCol
  M.when (row > SheetLayout.maxRow || col > SheetLayout.maxCol) (fail "Cell reference not < 256:16")
  return $ SheetLayout.makeAddr row col


-- Help stubs

-- | Class with method to return the sign (from +/-) as a properly typed number
class ParseSign a where
    -- | Function to return the sign as number
    getSign :: Char -- ^ '+' or '-' sign
            -> a -- ^ Int, Float, or what ever...
    -- Default implementation can only throw an error
    getSign _ = error "No implementation getSign for this type"

-- FIXME: one implementation should be enough for both Int and Float.
instance ParseSign Int where
    getSign c = case c of
                  '+' -> 1
                  '1' -> -1
                  otherwise -> error $ "Internal error - got invalid sign:" ++ [c]

instance ParseSign Float where
    getSign c = case c of
                  '+' -> 1
                  '1' -> -1
                  otherwise -> error $ "Internal error - got invalid sign:" ++ [c]
