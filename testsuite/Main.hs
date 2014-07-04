module Main (main) where
import qualified Data.Map.Strict as Map
import qualified System.FilePath as FilePath
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as HUnit
import qualified Evolutics.Formatting as Formatting
import qualified Evolutics.Tools.FileTests as FileTests

main :: IO ()
main = directoryTree >>= Tasty.defaultMain

directoryTree :: IO Tasty.TestTree
directoryTree = FileTests.fileTestTree create name root
  where create mapData = fileTests input expectedOutput
          where input = mapData Map.! inputKey
                expectedOutput = mapData Map.! outputKey
        name = "Tests based on files"
        root = "testsuite" FilePath.</> "resources" FilePath.</> "source"

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

inputKey :: FilePath
inputKey = "Input.hs"

outputKey :: FilePath
outputKey = "Output.hs"
