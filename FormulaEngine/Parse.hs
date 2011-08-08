-- | Author: Thorsten Rangwich
-- | See file LICENSE for details.
-- | This file defines the tree structure used for the formulas.

module FormulaEngine.Parse
(
 compile
)

where

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P
import qualified FormulaEngine.Functions.Numerics as NumFuncs
import qualified FormulaEngine.Registry as Reg
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>), (<?>))

-- | Function to parse a string and return a compiled FormulaTree
compile :: String -> T.FormulaTree
compile s = case (Parsec.parse term "" s) of
              Left err -> (T.TreeError (T.NamedError (show err)))
              Right v -> v


-- | Formula parser to be used by Parsec
term :: Parsec.Parser T.FormulaTree
term = do -- parse function call containing more terms
         (command, args) <- parseFunction
         return (T.Funcall (Reg.resolve command) args)
     <|> -- parse escaped string
       do
         word <- parseString
         Parsec.spaces
         return (T.Raw (P.PlString word))
     <|> --Parse a reference or anything else strange. FIXME: This should be replaced by reference parsing.
         -- This is just an example how to add a negative matching or a cell reference. Negative matching
         -- easily brings up funny problems and should not be done.
       do
         -- The right parantheses is necessary, otherwise the plain expressions in a paranthesis expression
         -- will not terminate properly
         c <- Parsec.noneOf ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '-', ')']
         fail ("Invalid character:" ++ (c:[]) ++ ". References or special treatment not yet implemented")
     <|> -- parse a number as float or integer. This has to be the last one to parse.
       do
         sign <- Parsec.option '+' (Parsec.oneOf ['+', '-'])
         prefix <- Parsec.many1 Parsec.digit -- could do without a digit as well with special treatment afterwards
         comma <- Parsec.option 'X' (Parsec.char '.')
         if comma == 'X'
           then
               do
                 Parsec.spaces
                 return (T.Raw (P.PlInt ((getSign sign) * (read prefix))))
           else
               do
                 Parsec.spaces
                 suffix <- Parsec.many1 Parsec.digit -- could do without a digit, see above
                 return (T.Raw (P.PlFloat ((getSign sign) * (read (prefix ++ "." ++ suffix)))))
     <?> "function term, number, string or reference"

parseFunction :: Parsec.Parser (String, [T.FormulaTree])
parseFunction = do
  Parsec.char '('
  Parsec.spaces
  command <- Parsec.many1 Parsec.letter
  Parsec.spaces
  args <- Parsec.many term
  Parsec.char ')'
  return (command, args)

-- FIXME: This does not escape double quotes yet.
-- | Parse String, double quote escapes a double quote. Take rest as is.
escapedChar :: Parsec.Parser Char
escapedChar = do
  c <- Parsec.noneOf ['"'] -- add newline or similar strange stuff?
  return c

parseString :: Parsec.Parser String
parseString = do
  Parsec.char '"'
  word <- Parsec.many escapedChar
  Parsec.char '"'
  return word

-- Help stubs
class ParseSign a where
    getSign :: Char -> a
    getSign _ = error "No implementation getSign for this type"

--FIXME: one implementation should be enough for both
instance ParseSign Int where
    getSign c = case c of
                  '+' -> 1
                  '1' -> -1
                  otherwise -> error ("Internal error - got invalid sign:" ++ (c:[]))

instance ParseSign Float where
    getSign c = case c of
                  '+' -> 1
                  '1' -> -1
                  otherwise -> error ("Internal error - got invalid sign:" ++ (c:[]))

