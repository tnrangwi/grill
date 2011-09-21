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

-- | Test all functions implemented currently. FIXME: There should be a unit test instead.
main :: IO ()
main = do
  print "Evaluating raw formula tree"
  let rawtree = T.Funcall NumFuncs.add [(T.Raw (P.PlInt 1)), (T.Raw (P.PlInt 2))]
  print (Eval.calcTree rawtree)
  print "Done!"

  print "Compiling and evaluating add tree with int - should give 6"
  let stringtree1 = Parse.compileTree "(add 1 (add 2 3))"
  print (Eval.calcTree stringtree1)

  print "Compiling and evaluating add tree with float - should give 6.3"
  let stringtree2 = Parse.compileTree "(add 1.0 (add 2.1 3.2))"
  print (Eval.calcTree stringtree2)

  print "Compiling and evaluating string function containing escaped quotes"
  let stringtree3 = Parse.compileTree "(conc \"A\"\"\" \"BC\")"
  print (Eval.calcTree stringtree3)

  print "Compiling tree with references - will fail in eval"
  let reftree1 = Parse.compileTree "'1:1"
  print (Eval.calcTree reftree1)

  print "All done successfully"
