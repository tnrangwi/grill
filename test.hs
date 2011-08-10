#!/usr/bin/runghc
-- Author: Thorsten Rangwich
-- See file LICENSE for details.
-- This file defines the tree structure used for the formulas.

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P
import qualified FormulaEngine.Evaluate as Eval
import qualified FormulaEngine.Functions.Numerics as NumFuncs
import qualified FormulaEngine.Parse as Parse

-- Fixme: How to treat different input types accordingly?

-- | Test function to add values

main :: IO ()
main = do
  print "Evaluating raw formula tree"
  let rawtree = T.Funcall NumFuncs.add [(T.Raw (P.PlInt 1)), (T.Raw (P.PlInt 2))]
  print (Eval.eval rawtree)
  print "Done!"
  print "Compiling and evaluating String tree"
  let stringtree = Parse.compile "(add 1 (add 2 3))"
  print (Eval.eval stringtree)
  print "Compiling and evaluating string function"
  let stringtree2 = Parse.compile "(conc \"A\" \"BC\")"
  print (Eval.eval stringtree2)
  print "Done"