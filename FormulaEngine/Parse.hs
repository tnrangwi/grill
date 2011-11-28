-- | This file implements parsing of both full sheets and single formula trees.
-- 
-- | Author: Thorsten Rangwich. See file <../LICENSE> for details.

module FormulaEngine.Parse
(
 compileTree,
 compileSheet,
 compileEditCell
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

{-
 FIXME: There should be a function using a handle instead of a string for sheet compilation.
        Creating the string is very expensive.
 FIXME: Return something else in exported parser functions on error if errors get more sophisticated?
 FIXME: Parse empty cells properly in sheet parser.
 FIXME: Return sheet header information. Enhance header in separated major, minor, micro.
 FIXME: Change sheet format again (leading tabs) to return empty rows properly.
 FIXME: Possibly remove try statetement, this is expensive and it should be possible without that.
 FIXME: Enhance number parser (could do without digits or leading numbers, does not respect exponents).
-}


---------------------------
-- Exporter main functions
---------------------------

-- | Function to parse a string into a compiled FormulaTree.
compileTree :: String -- ^ Input string -- as given in a cell.
            -> T.FormulaTree -- ^ Compiled formula tree.
compileTree s = case Parsec.parse (realSpaces >> term) "" s of
                  Left err -> T.TreeError . T.NamedError . show $ err
                  Right v -> v

-- | Function to parse a whole sheet.
compileSheet :: String -- ^ Input String. This may change to something else in the future.
             -> Either String Sheet.RawSheet
compileSheet s = case Parsec.parse sheet "" s of
                   Left err -> Left $ show err
                   Right v -> Right v

-- | Function to compile an address and a new tree to put into address.
compileEditCell :: String -- ^ Input String.
                -> Either String (SheetLayout.Address, T.FormulaTree)
compileEditCell s = case Parsec.parse editCellExpr "" s of
                      Left err -> Left $ show err
                      Right v -> Right v


------------------------------------------------
-- Parser building blocks to parse a whole sheet
------------------------------------------------

-- | Parser help function for parsing a whole sheet.
sheet :: Parsec.Parser Sheet.RawSheet
sheet = do
          header <- sheetHeader
          rows <- Parsec.endBy sheetRow eol
                                -- FIXME: This is mad. Read in the whole sheet and convert it afterwards.
                                -- Use a parser with a state to fill sheet on the fly. Or solve this properly
                                -- with lazy evaluation (maybe it even is already).
          let res = Sheet.buildSheet header rows
          case res of
            Left err -> fail err
            Right r -> return r
      <?> "This does not have sheet structure"

          
-- | Parse sheet header
sheetHeader :: Parsec.Parser Sheet.RawHeader
sheetHeader = do
                Version.parseMagicBytes
                format <- Version.parseFormat
                version <- Version.parseVersion
                checksum <- Version.parseChecksum
                Version.parseEndOfHeader
                eol
                let f = Version.formatString
                    v = Version.versionString
                M.when (format > f || version > v) (fail "Your grill version is too old to open this")
                return $ 
                       Sheet.addHeaderProperties
                                [("format", P.PlString format),
                                 ("version", P.PlString version),
                                 ("checksum", P.PlString checksum)]
                                Sheet.emptyRawHeader 
            <?> "expecting sheet header"


-- | Parse one sheet row.
sheetRow :: Parsec.Parser [T.FormulaTree]
sheetRow = Parsec.sepBy term (Parsec.char '\t')
         <?> "expecting sheet row"

-----------------------------
-- Parse cell edit expression
-----------------------------

-- | Parse edit cell expression: A combination of address and term
editCellExpr :: Parsec.Parser (SheetLayout.Address, T.FormulaTree)
editCellExpr = do
  addr <- parseReferenceContent
  Parsec.spaces
  tree <- term
  return (addr, tree) -- FIXME: Error message?
 
---------------------------------------------------------------------
-- Parser building blocks to parse a single cell expression / a tree.
---------------------------------------------------------------------

-- | Parse a single term, i. e. cell content as found in sheet or in cell editor).
term :: Parsec.Parser T.FormulaTree
term = do -- parse function call containing more terms
         (command, args) <- parseFunction
         realSpaces
         return $ T.Funcall (Reg.resolve command) args
     <|> -- parse escaped string
       do
         word <- parseString
         realSpaces
         return . T.Raw . P.PlString $ word
     <|> -- parse reference to another tree
       do
         addr <- parseReference
         realSpaces
         return $ T.Reference addr
     <|> -- parse a number as float or integer. This has to be the last one to parse.
       do
         number <- parseNumber
         realSpaces
         return . T.Raw $ number
     <|> -- parse empty cell. FIXME: Does only accept a cell with at least one space
       do
         Parsec.char ' '
         realSpaces
         return . T.Raw $ P.PlEmpty
     <?> "function term, number, string, reference or empty"


-- | Parse a function call within a tree.
parseFunction :: Parsec.Parser (String, [T.FormulaTree])
parseFunction = do
  Parsec.char '('
  realSpaces
  command <- Parsec.many1 Parsec.letter
  realSpaces
  args <- Parsec.many term
  Parsec.char ')'
  return (command, args)

-- | Parse a reference to another tree, addressed as external cell address.
-- This either is called within a tree parse or directly within an edit cell command (where no leading single
-- quote is given).
parseReferenceContent :: Parsec.Parser SheetLayout.Address
parseReferenceContent = do
  stringRow <- Parsec.many1 Parsec.digit
  Parsec.char ':'
  stringCol <- Parsec.many1 Parsec.digit
  let row = read stringRow
  let col = read stringCol
  M.when (row > SheetLayout.maxRow || col > SheetLayout.maxCol) (fail "Cell reference not < 256:16")
  return $ SheetLayout.makeAddr row col


-- | Parse reference content. This is marked with a leading single quote to distinguish it from values and functions.
parseReference :: Parsec.Parser SheetLayout.Address
parseReference = do
  Parsec.char '\''
  parseReferenceContent


----------------------------------------------------------
-- Building blocks to parse "raw" values (part of a tree).
----------------------------------------------------------


-- | Parse a string enclosed in double quotes.
parseString :: Parsec.Parser String
parseString = do
  Parsec.char '"'
  word <- Parsec.many escapedChar
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



-- Parser help stubs

-- | Parse "our" definition of eol. Will change, I see no reason why this should be text editor
-- compatible.
eol :: Parsec.Parser ()
eol = (Parsec.string "\n" <|> Parsec.string "\r\n" <|> Parsec.string "\r") >> return ()

-- | Parse any number of spaces. Standard whitespace is not suitable because tabs are used
-- to separate the cells within a row. Spaces will be possible within the cell editor.
realSpaces :: Parsec.Parser ()
realSpaces = Parsec.many (Parsec.char ' ') >> return ()

-- | Parse String, double quote escapes a double quote. Take rest as is.
escapedChar :: Parsec.Parser Char
escapedChar = do
                Parsec.try $ Parsec.string "\"\""
                return '"'
            <|>
              Parsec.noneOf "\""
            <?> "Parsing of string changed - internal error. You will never be here."


--------------
-- Help stubs.
--------------


-- | Function returning the sign (from +/-) as a properly typed number.
-- The definition of symbols like 1 in Haskell allows to do it in one function
-- without a typeclass for all instances of Num.
getSign :: (Num a) => 
    Char -- ^ '+' or '-' sign
    -> a -- ^ Any type compatible with Num so the implementation will work.
getSign c = case c of
              '+' -> 1
              '-' -> -1
              otherwise -> error $ "Internal error - got invalid sign:" ++ [c]
