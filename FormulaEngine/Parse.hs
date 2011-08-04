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
import qualified FormulaEngine.Functions.Registry as Reg
import qualified Text.ParserCombinators.Parsec as Parsec
import Text.ParserCombinators.Parsec ((<|>))

-- | Function to parse a string and return a compiled FormulaTree
compile :: String -> T.FormulaTree
compile s = case (Parsec.parse term "" s) of
              Left err -> (T.TreeError (T.NamedError (show err)))
              Right v -> v


-- | Formula parser to be used by Parsec
term :: Parsec.Parser T.FormulaTree
term = do -- parse enother term
         Parsec.char '('
         Parsec.spaces --"skipMany space instead?
         command <- Parsec.many1 Parsec.letter
         Parsec.spaces
         args <- Parsec.many term
         Parsec.char ')'
         Parsec.spaces
         return (T.Funcall (Reg.resolve command) args)
     <|> -- parse escaped string
       do
         Parsec.char '"'
         word <- Parsec.many escapedChar
         Parsec.char '"'
         Parsec.spaces
         return (T.Raw (P.PlString word))
     <|> -- parse a number as float or integer
       do
         sign <- Parsec.option '+' (Parsec.oneOf ['+', '-'])
         prefix <- Parsec.many1 Parsec.digit -- could do without a digit as well with special treatment afterwards
         comma <- Parsec.option 'X' (Parsec.char '.')
         if comma == 'X'
           then
               do
                 Parsec.spaces
                 -- error ("Read:" ++ ((sign:[]) ++ prefix))
                 return (T.Raw (P.PlInt ((if sign == '-' then -1 else 1) * (read prefix))))
           else
               do
                 Parsec.spaces
                 suffix <- Parsec.many1 Parsec.digit -- could do without a digit, see above
                 return (T.Raw (P.PlFloat ((if sign == '-' then -1 else 1) * (read (prefix ++ "." ++ suffix)))))
         
             

-- FIXME: This does not escape double quotes yet.
-- | Parse String, double quote escapes a double quote. Take rest as is.
escapedChar :: Parsec.Parser Char
escapedChar = do
  c <- Parsec.noneOf ['"'] -- add newline or similar strange stuff?
  return c


{- Not needed, I suppose...
number :: Char -> Parsec.Parser -> Either Int Float
number ind = do
  case ind of
         case ind of
           Left i -> case sign of
                       '-' -> return T.Raw (P.PlInt i * -1)
                       '+' -> return T.Raw (P.PlInt i)
                       '.' -> return T.Raw (P.PlInt 0) -- this cannot happen
                       otherwise -> return T.Raw (P.PlInt ((read sign) * 10 ^ (exponent i) + i))
           Right f -> case signe of
                        '-' -> return T.Raw (P.PlFloat f * -1.0)
                        '+' return R.Raw (P.PlFloat f)
                        '.' return T.Raw ()
                        otherwise -> return T.Raw (P.PlInt ((read signe) * 10 ^ (exponent f) + f))
         -- comment out
         case sign of
           '-' -> let (fac, hasVk) = (-1, True)
           '+' -> let (fac, hasVk) = (1, True)
           '.' -> let (fac, hasVk) = (1, False)
           otherwise -> let (fac, vk) = (read sign :: Int, True)
         if vk then
             dkf <- many digit
         else
             vk = 0
-}