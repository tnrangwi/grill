-- | This file defines tests tree evaluation and parser functions.
-- 
--   Author: Thorsten Rangwich. See file <../LICENSE> for details.

import Test.QuickCheck.Batch

import qualified Data.List as DL

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P
import qualified FormulaEngine.Evaluate as Eval
import qualified FormulaEngine.Functions.Numerics as NumFuncs
import qualified FormulaEngine.Functions.StringFuncs as StringFuncs
import qualified FormulaEngine.Parse as Parse


-- | Convert a tree function into an Int function.
integerFuncWrapper :: ([P.Plain] -> P.Plain) -> ([Int] -> Int)
integerFuncWrapper tFunc = P.get . Eval.calcTree . T.Funcall tFunc . map (T.Raw . P.PlInt)

floatFuncWrapper :: ([P.Plain] -> P.Plain) -> ([Float] -> Float)
floatFuncWrapper tFunc = (P.get . Eval.calcTree . T.Funcall tFunc . map (T.Raw . P.PlFloat))

stringFuncWrapper :: ([P.Plain] -> P.Plain) -> ([String] -> String)
stringFuncWrapper tFunc = (P.get . Eval.calcTree . T.Funcall tFunc . map (T.Raw . P.PlString))

-- | Integer add function should match results of builtin (+) for Int.
prop_addInt :: [Int] -> Bool
prop_addInt xs = (integerFuncWrapper NumFuncs.add) xs == (foldl (\x y -> x + y) 0) xs

-- | String concatenation function.
prop_concString :: [String] -> Bool
prop_concString xs = (stringFuncWrapper StringFuncs.conc) xs == concat xs

-- | Recursive calls.
prop_recursive :: Bool
prop_recursive = (P.get . Eval.calcTree . Parse.compileTree) "(add 1 (add 2 3))" == (6 :: Int)

escape :: Char -> String
escape x = if x == '"' then "\"\"" else [x]

quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- | Leading spaces.
prop_leadingSpaces :: Int -> String -> Bool
prop_leadingSpaces n s = (P.get . Eval.calcTree . Parse.compileTree) (concat [(DL.replicate n ' '), "\"", concat (map escape s), "\""]) == s

-- | Arbitrary strings.
prop_string :: String -> Bool
prop_string s = ((P.get . Eval.calcTree . Parse.compileTree . quote . concat . map escape) s) == s

-- | References are invalid in calcTrees.
prop_invalidReference :: Bool
prop_invalidReference = (P.checkError . Eval.calcTree . Parse.compileTree) "'1:1"


options = TestOptions { no_of_tests = 200,
                        length_of_tests = 1,
                        debug_tests = False }

-- | Run the tests
main :: IO ()
main = do
  runTests "simple" options
               [ run prop_addInt
               --, run prop_concString
               , run prop_recursive
               --, run prop_leadingSpaces
               --, run prop_string
               , run prop_invalidReference
               ]
