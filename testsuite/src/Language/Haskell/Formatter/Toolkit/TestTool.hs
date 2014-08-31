{-|
Description : Testing utilities
-}
module Language.Haskell.Formatter.Toolkit.TestTool
       (testingError, codeHintTest, documentationTest) where
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.HLint as HLint
import qualified Test.DocTest as DocTest
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

testingError :: Tasty.TestName -> String -> Tasty.TestTree
testingError name = HUnit.testCase name . HUnit.assertFailure

codeHintTest :: FilePath -> Tasty.TestTree
codeHintTest file = HUnit.testCase file assertion
  where assertion
          = do errors <- HLint.hlint arguments
               HUnit.assertBool (showLines errors) $ null errors
        arguments = [suppressFeedback, file]
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
