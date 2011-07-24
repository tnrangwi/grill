#!/usr/bin/runghc

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P

-- Fixme: How to treat different input types accordingly?

-- | Test function to add values
add :: [P.Plain] -> P.Plain
add = foldl step P.PlEmpty
    where step P.PlEmpty p@(P.PlInt v) = p
          step (P.PlInt v1) (P.PlInt v2) = P.PlInt (v1 + v2)
          step p@(P.PlError v) _ = p
          step _ _ = P.PlError "Invalid data type in add"

main :: IO ()
main = do
  let t1 = T.Raw P.PlEmpty
  let t2 = T.Funcall add [(T.Raw (P.PlInt 1)), (T.Raw (P.PlInt 2))]
  print (T.eval t2)
  print "Done"