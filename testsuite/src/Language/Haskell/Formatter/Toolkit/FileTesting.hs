module Language.Haskell.Formatter.Toolkit.FileTesting
       (fileTestTree) where
import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Toolkit.DirectoryData
       as DirectoryData
import qualified Language.Haskell.Formatter.Toolkit.MapTree
       as MapTree
import qualified Test.Tasty as Tasty

fileTestTree ::
             (Either Exception.IOException (Map.Map FilePath String) ->
                [Tasty.TestTree])
               -> Tasty.TestName -> FilePath -> IO Tasty.TestTree
fileTestTree = directoryTestTree readFile

directoryTestTree ::
                    (Monoid.Monoid a) =>
                    (FilePath -> IO a) ->
                      (Either Exception.IOException (Map.Map FilePath a) ->
                         [Tasty.TestTree])
                        -> Tasty.TestName -> FilePath -> IO Tasty.TestTree
directoryTestTree create createTests name
  = fmap (testTree createTests name) .
      DirectoryData.createTree create

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
