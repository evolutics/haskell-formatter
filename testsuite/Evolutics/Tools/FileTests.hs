module Evolutics.Tools.FileTests (fileTestTree) where
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Test.Tasty as Tasty
import qualified Evolutics.Tools.DirectoryData as DirectoryData
import qualified Evolutics.Tools.MapTree as MapTree

fileTestTree ::
             (Map.Map FilePath String -> [Tasty.TestTree]) ->
               Tasty.TestName -> FilePath -> IO Tasty.TestTree
fileTestTree = directoryTestTree readFile

directoryTestTree ::
                    (Monoid.Monoid a) =>
                    (FilePath -> IO a) ->
                      (Map.Map FilePath a -> [Tasty.TestTree]) ->
                        Tasty.TestName -> FilePath -> IO Tasty.TestTree
directoryTestTree create createTests name
  = fmap (checkedTestTree createTests name) .
      DirectoryData.createTree create

checkedTestTree ::
                  (Show a) =>
                  (b -> [Tasty.TestTree]) ->
                    Tasty.TestName ->
                      MapTree.MapTree FilePath (Either a b) -> Tasty.TestTree
checkedTestTree createTests = testTree createCheckedTests
  where createCheckedTests (Left exception) = error $ show exception
        createCheckedTests (Right leafMap) = createTests leafMap

testTree ::
         (a -> [Tasty.TestTree]) ->
           Tasty.TestName ->
             MapTree.MapTree Tasty.TestName a -> Tasty.TestTree
testTree createTests name (MapTree.Leaf value)
  = Tasty.testGroup name $ createTests value
testTree createTests name
  (MapTree.Node (MapTree.MapForest children))
  = Tasty.testGroup name $ mapOrder (testTree createTests) children
  where mapOrder function = Map.elems . Map.mapWithKey function
