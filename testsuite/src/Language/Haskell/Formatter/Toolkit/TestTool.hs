{-|
Description : Testing utilities
-}
module Language.Haskell.Formatter.Toolkit.TestTool
       (testingError, standardSourceCodeTest, documentationTest) where
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.HLint as HLint
import qualified Test.DocTest as DocTest
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

testingError :: Tasty.TestName -> String -> Tasty.TestTree
testingError name = HUnit.testCase name . HUnit.assertFailure

standardSourceCodeTest :: FilePath -> Tasty.TestTree
standardSourceCodeTest file = HUnit.testCase file assertion
  where assertion = sequence_ assertions
        assertions = [lineLengthAssertion file, codeHintAssertion file]

lineLengthAssertion :: FilePath -> HUnit.Assertion
lineLengthAssertion file = readFile file >>= test
  where test string = HUnit.assertBool message $ null indices
          where message
                  = concat
                      ["The following lines are longer than ", show lengthLimit,
                       " characters: ", show indices]
                indices
                  = [index | (line, index) <- indexedLines,
                     length line > lengthLimit]
                indexedLines = zip (lines string) [baseLine ..]
        lengthLimit = 80
        baseLine = 1 :: Integer

codeHintAssertion :: FilePath -> HUnit.Assertion
codeHintAssertion file
  = do errors <- HLint.hlint arguments
       HUnit.assertBool (showLines errors) $ null errors
  where arguments = [suppressFeedback, file]
        suppressFeedback = "--quiet"
        showLines = unlines . fmap show

documentationTest :: [FilePath] -> FilePath -> Tasty.TestTree
documentationTest searchFolders file = HUnit.testCase file assertion
  where assertion = DocTest.doctest arguments
        arguments = [searchPathAppendix, file]
        searchPathAppendix = Monoid.mappend "-i" appendedPaths
        appendedPaths
          = List.intercalate [pathSeparator] $ fmap escape searchFolders
        pathSeparator = ':'
        escape = (>>= replace)
        replace character = Monoid.mappend prefix [character]
          where prefix = ['\\' | character == pathSeparator]
