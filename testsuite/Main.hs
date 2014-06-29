module Main (main) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Evolutics.Formatting as Formatting
import qualified Evolutics.Tools.FileTests as FileTests

main :: IO ()
main = directoryTree >>= Tasty.defaultMain

directoryTree :: IO Tasty.TestTree
directoryTree = FileTests.directoryTestTree keys create name root
  where keys = Set.fromList [inputKey, outputKey]
        create mapData = fileTests input expectedOutput
          where input = mapData Map.! inputKey
                expectedOutput = mapData Map.! outputKey
        name = "Tests based on files"
        root = "testsuite" FilePath.</> "resources" FilePath.</> "source"

inputKey :: FilePath
inputKey = "Input.hs"

outputKey :: FilePath
outputKey = "Output.hs"

fileTests :: String -> String -> [Tasty.TestTree]
fileTests input expectedOutput
  = [HUnit.testCase "Base" base,
     HUnit.testCase "Idempotence" idempotence]
  where base = testFormatting inputKey input expectedOutput
        idempotence
          = testFormatting outputKey expectedOutput expectedOutput

testFormatting :: FilePath -> String -> String -> HUnit.Assertion
testFormatting inputFile input expectedOutput
  = case Formatting.formatSource (Just inputFile) input of
        Left _ -> HUnit.assertFailure "Not formatted"
        Right actualOutput -> actualOutput HUnit.@?= expectedOutput
