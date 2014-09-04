module Language.Haskell.Formatter.Internal.Tests (tests) where
import qualified Language.Haskell.Formatter as Formatter
import qualified Language.Haskell.Formatter.Internal.StyleFileFormat
       as StyleFileFormat
import qualified Language.Haskell.Formatter.Internal.TreeFormat as TreeFormat
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit

tests :: IO Tasty.TestTree
tests = return testDefaultStyleFile

testDefaultStyleFile :: Tasty.TestTree
testDefaultStyleFile = HUnit.testCase "Default style file" assertion
  where assertion
          = do maybeStyle <- getStyle
               case maybeStyle of
                   Left errorMessage -> HUnit.assertFailure errorMessage
                   Right style -> style HUnit.@?= defaultStyle
        getStyle
          = TreeFormat.parseYamlFile StyleFileFormat.treeFormat defaultStyle
              file
        defaultStyle
          = Formatter.configurationStyle Formatter.defaultConfiguration
        file = "default_style.yaml"
