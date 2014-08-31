{-|
Description : Root of test suite
-}
module Main (main) where
import qualified Control.Applicative as Applicative
import qualified Data.Set as Set
import qualified Language.Haskell.Formatter.Tests as Formatter
import qualified Language.Haskell.Formatter.Toolkit.TestTool
       as TestTool
import qualified System.FilePath as FilePath
import qualified System.FilePath.Find as Find
import qualified Test.Tasty as Tasty

main :: IO ()
main
  = sequence tests >>= Tasty.defaultMain . Tasty.testGroup "Root"

tests :: [IO Tasty.TestTree]
tests = [codeHintTests, documentationTests, Formatter.tests]

codeHintTests :: IO Tasty.TestTree
codeHintTests
  = createTestTree TestTool.codeHintTest Find.always
      "Code hint tests"

createTestTree ::
               (FilePath -> Tasty.TestTree) ->
                 Find.RecursionPredicate -> Tasty.TestName -> IO Tasty.TestTree
createTestTree test recurse rootName
  = do files <- concat Applicative.<$>
                  mapM (collectSourceFiles recurse) roots
       return . Tasty.testGroup rootName $ fmap test files

collectSourceFiles ::
                   Find.RecursionPredicate -> FilePath -> IO [FilePath]
collectSourceFiles recurse = Find.find recurse isSourceFile
  where isSourceFile = isFile Find.&&? hasSourceExtension
        isFile = Find.fileType Find.==? Find.RegularFile
        hasSourceExtension
          = fmap (`Set.member` sourceExtensions) Find.extension
        sourceExtensions = Set.fromList [".hs", ".lhs"]

roots :: [FilePath]
roots
  = ["src" FilePath.</> "library", "src" FilePath.</> "executable",
     "testsuite" FilePath.</> "src"]

documentationTests :: IO Tasty.TestTree
documentationTests
  = createTestTree (TestTool.documentationTest roots) noRecursion
      "Documentation tests"
  where noRecursion = Find.depth Find.==? rootDepth
        rootDepth = 0
