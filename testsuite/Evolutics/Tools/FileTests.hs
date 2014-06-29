module Evolutics.Tools.FileTests (directoryTestTree) where
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Test.Tasty as Tasty
import qualified Evolutics.Tools.DirectoryData as DirectoryData
import qualified Evolutics.Tools.MapTree as MapTree

directoryTestTree ::
                  Set.Set FilePath ->
                    (Map.Map FilePath String -> [Tasty.TestTree]) ->
                      Tasty.TestName -> FilePath -> IO Tasty.TestTree
directoryTestTree keys createTests name
  = fmap (checkedTestTree createTests name) .
      DirectoryData.createTree readFile keys

checkedTestTree ::
                (Map.Map FilePath String -> [Tasty.TestTree]) ->
                  Tasty.TestName ->
                    MapTree.MapTree FilePath
                      ([DirectoryData.Exception], Map.Map FilePath String)
                      -> Tasty.TestTree
checkedTestTree createTests = testTree createCheckedTest
  where createCheckedTest ([], leafMap) = createTests leafMap
        createCheckedTest (exceptions, _)
          = error . unlines $ map show exceptions

testTree ::
         (a -> [Tasty.TestTree]) ->
           Tasty.TestName ->
             MapTree.MapTree Tasty.TestName a -> Tasty.TestTree
testTree createTests name (MapTree.Leaf value)
  = Tasty.testGroup name $ createTests value
testTree createTests name (MapTree.Node children)
  = Tasty.testGroup name $
      mapMapOrdered (testTree createTests) children
  where mapMapOrdered function = Map.elems . Map.mapWithKey function
