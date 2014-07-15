module Main (main) where
import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Evolutics.Formatting as Formatting
import qualified Evolutics.Tools.FileTests as FileTests

main :: IO ()
main = directoryTree >>= Tasty.defaultMain

directoryTree :: IO Tasty.TestTree
directoryTree = FileTests.fileTestTree create name root
  where name = "Tests based on files"
        root = "testsuite" FilePath.</> "resources" FilePath.</> "source"

create ::
       Either Exception.IOException (Map.Map FilePath String) ->
         [Tasty.TestTree]
create (Left exception) = failure "IO exception" $ show exception
create (Right testMap)
  = if actualKeys == expectedKeys then fileTests input expectedOutput
      else failure "Set of filenames" message
  where actualKeys = Map.keysSet testMap
        expectedKeys = Set.fromList [inputKey, outputKey]
        input = testMap Map.! inputKey
        expectedOutput = testMap Map.! outputKey
        message
          = Monoid.mconcat
              ["The filenames are ", setString actualKeys, " instead of ",
               setString expectedKeys, "."]
        setString = show . Set.elems

failure :: Tasty.TestName -> String -> [Tasty.TestTree]
failure name message
  = [HUnit.testCase name $ HUnit.assertFailure message]

inputKey :: FilePath
inputKey = "Input.hs"

outputKey :: FilePath
outputKey = "Output.hs"

fileTests :: String -> String -> [Tasty.TestTree]
fileTests input expectedOutput
  = [HUnit.testCase "Formatting once" base,
     HUnit.testCase "Idempotence" idempotence]
  where base = testFormatting inputKey input expectedOutput
        idempotence
          = testFormatting outputKey expectedOutput expectedOutput

testFormatting :: FilePath -> String -> String -> HUnit.Assertion
testFormatting inputFile input expectedOutput
  = case Formatting.formatSource (Just inputFile) input of
        Left message -> HUnit.assertFailure .
                          Monoid.mappend "Unexpected formatter failure: "
                          $ show message
        Right actualOutput -> actualOutput HUnit.@?= expectedOutput
