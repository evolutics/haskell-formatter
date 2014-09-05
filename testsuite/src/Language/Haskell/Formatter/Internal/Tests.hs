{-|
Description : Tests for internal parts
-}
module Language.Haskell.Formatter.Internal.Tests (tests) where
import qualified Data.List as List
import qualified Language.Haskell.Formatter as Formatter
import qualified Language.Haskell.Formatter.Internal.StyleFileFormat
       as StyleFileFormat
import qualified Language.Haskell.Formatter.Internal.TreeFormat as TreeFormat
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

tests :: IO Tasty.TestTree
tests = return testDefaultStyleFile

testDefaultStyleFile :: Tasty.TestTree
testDefaultStyleFile = Tasty.testGroup name defaultStyleFileTests
  where name = "Default style file"
        defaultStyleFileTests
          = [testDefaultStyleFileUsesDefaults,
             testReadmeContainsDefaultStyleFile]

testDefaultStyleFileUsesDefaults :: Tasty.TestTree
testDefaultStyleFileUsesDefaults = HUnit.testCase name assertion
  where name = "Uses defaults"
        assertion
          = do maybeStyle <- getStyle
               case maybeStyle of
                   Left errorMessage -> HUnit.assertFailure errorMessage
                   Right style -> style HUnit.@?= defaultStyle
        getStyle
          = TreeFormat.parseYamlFile StyleFileFormat.treeFormat defaultStyle
              defaultStyleFile
        defaultStyle
          = Formatter.configurationStyle Formatter.defaultConfiguration

defaultStyleFile :: FilePath
defaultStyleFile
  = "testsuite" FilePath.</> "resources" FilePath.</> "examples" FilePath.</>
      "default_style.yaml"

testReadmeContainsDefaultStyleFile :: Tasty.TestTree
testReadmeContainsDefaultStyleFile = HUnit.testCase name assertion
  where name = "Is contained in readme"
        assertion
          = do styleString <- readFile defaultStyleFile
               readme <- readFile readmeFile
               let isContained = isContainedInReadme styleString readme
               HUnit.assertBool message isContained
        readmeFile = "README.rst"
        message
          = concat
              [show readmeFile, " must contain ", show defaultStyleFile,
               ", but it does not."]

isContainedInReadme :: String -> String -> Bool
isContainedInReadme file = List.isInfixOf preparedFile
  where preparedFile = indent file
        indent = unlines . fmap (indentation ++) . lines
        indentation = "    "
