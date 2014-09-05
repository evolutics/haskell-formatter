{-|
Description : Test data based on file trees
-}
module Language.Haskell.Formatter.Toolkit.FileTesting (fileTestForest) where
import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Internal.MapTree as MapTree
import qualified Language.Haskell.Formatter.Toolkit.FileTree as FileTree
import qualified Test.Tasty as Tasty

fileTestForest ::
               (Either Exception.IOException (Map.Map FilePath String) ->
                  [Tasty.TestTree])
                 -> FilePath -> IO [Tasty.TestTree]
fileTestForest = folderTestForest readFile

folderTestForest ::
                   (Monoid.Monoid a) =>
                   (FilePath -> IO a) ->
                     (Either Exception.IOException (Map.Map FilePath a) ->
                        [Tasty.TestTree])
                       -> FilePath -> IO [Tasty.TestTree]
folderTestForest create createTests rootFolder
  = do fileForest <- FileTree.collectFiles create rootFolder
       return . createTestForest $ MapTree.summarizeLeaves fileForest
  where createTestForest = testForest createTests

testForest ::
           (a -> [Tasty.TestTree]) ->
             MapTree.MapTree Tasty.TestName a -> [Tasty.TestTree]
testForest createTests (MapTree.Leaf value) = createTests value
testForest createTests (MapTree.Node forest)
  = Map.elems $ Map.mapWithKey testTree forest
  where testTree label = Tasty.testGroup label . testForest createTests
