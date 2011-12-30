-- | This file defines tests tree evaluation and parser functions.
-- 
-- Author: Thorsten Rangwich. See file <../LICENSE> for details.
-- 
-- This needs QuickCheck in at least version 2. Otherwise you have to replace QuickCheck2 with QuickCheck and
-- either implement an instance for Arbitrary Char or leave out the string tests.

import qualified Test.Framework as Fw
import qualified Test.Framework.Providers.QuickCheck2 as FQc

import qualified Data.List as DL
import qualified Data.Monoid as Monoid

import qualified Tree.FormulaTree as T
import qualified Data.Plain as P
import qualified FormulaEngine.Evaluate as Eval
import qualified FormulaEngine.Functions.Numerics as NumFuncs
import qualified FormulaEngine.Functions.StringFuncs as StringFuncs
import qualified FormulaEngine.Parse as Parse


-- | Convert a tree function into an Int function.
integerFuncWrapper :: ([P.Plain] -> P.Plain) -> [Int] -> Int
integerFuncWrapper tFunc = P.get . Eval.calcTree . T.Funcall (T.NamedFunction "f" tFunc) . map (T.Raw . P.PlInt)

floatFuncWrapper :: ([P.Plain] -> P.Plain) -> [Float] -> Float
floatFuncWrapper tFunc = P.get . Eval.calcTree . T.Funcall (T.NamedFunction "f" tFunc) . map (T.Raw . P.PlFloat)

stringFuncWrapper :: ([P.Plain] -> P.Plain) -> [String] -> String
stringFuncWrapper tFunc = P.get . Eval.calcTree . T.Funcall (T.NamedFunction "f" tFunc) . map (T.Raw . P.PlString)

-- | Integer add function should match results of builtin (+) for Int.
prop_addInt :: [Int] -> Bool
prop_addInt xs = integerFuncWrapper NumFuncs.add xs == sum xs

-- | String concatenation function.
prop_concString :: [String] -> Bool
prop_concString xs = stringFuncWrapper StringFuncs.conc xs == concat xs

-- | Recursive calls.
prop_recursive :: Bool
prop_recursive = (P.get . Eval.calcTree . Parse.compileTree) "(add 1 (add 2 3))" == (6 :: Int)

escape :: Char -> String
escape x = if x == '"' then "\"\"" else [x]

quote :: String -> String
quote s = "\"" ++ s ++ "\""

-- | Leading spaces.
prop_leadingSpaces :: Int -> String -> Bool
prop_leadingSpaces n s =
    (P.get . Eval.calcTree . Parse.compileTree) (concat [DL.replicate (n `mod` 20) ' ', "\"", concatMap escape s, "\""]) == s

-- | Arbitrary strings.
prop_string :: String -> Bool
prop_string s = (P.get . Eval.calcTree . Parse.compileTree . quote . concatMap escape) s == s

-- | References are invalid in calcTrees.
prop_invalidReference :: Bool
prop_invalidReference = (P.checkError . Eval.calcTree . Parse.compileTree) "'1:1"


-- | Example for test options used below. To make them the default, a top level group can be defined
-- and tests can be nested. This is *NOT* intuitive and took me some hours to find out how this works.
-- There is no single example for that in order to migrate this stuff from old QuickCheck!!!
options :: Fw.TestOptions
options = Monoid.mempty { Fw.topt_maximum_generated_tests = Just 200
                        , Fw.topt_maximum_unsuitable_generated_tests = Just 1 }


-- | Run the tests
main :: IO ()
main = Fw.defaultMain tests

-- | The tests. Test can be nested to reuse test options.
tests = [
 Fw.plusTestOptions options (Fw.testGroup "Integer tests" [
        FQc.testProperty "add Int" prop_addInt
       ]),
 Fw.testGroup "Miscellaneous tests" [
        FQc.testProperty "recursive" prop_recursive,
        FQc.testProperty "invalid ref" prop_invalidReference
       ],
 Fw.testGroup "String tests" [
       FQc.testProperty "concatenate" prop_concString,
       FQc.testProperty "spaces" prop_leadingSpaces,
       FQc.testProperty "string" prop_string
      ]]

