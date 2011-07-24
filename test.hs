#!/usr/bin/runghc

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P
import qualified FormulaEngine.Evaluate as Eval
import qualified FormulaEngine.Functions.Numerics as NumFuncs

-- Fixme: How to treat different input types accordingly?

-- | Test function to add values

main :: IO ()
main = do
  let tree = T.Funcall NumFuncs.add [(T.Raw (P.PlInt 1)), (T.Raw (P.PlInt 2))]
  print (Eval.eval tree)
  print "Done"