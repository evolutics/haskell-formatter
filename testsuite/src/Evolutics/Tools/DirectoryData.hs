module Evolutics.Tools.DirectoryData (createTree) where
import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified System.Directory.Tree as Tree
import qualified Evolutics.Tools.MapTree as MapTree

createTree ::
             (Monoid.Monoid a) =>
             (FilePath -> IO a) ->
               FilePath ->
                 IO
                   (MapTree.MapTree FilePath
                      (Either Exception.IOException (Map.Map FilePath a)))
createTree create
  = fmap MapTree.summarizeLeaves . collectFiles create

collectFiles ::
             (FilePath -> IO a) ->
               FilePath ->
                 IO (MapTree.MapForest FilePath (Either Exception.IOException a))
collectFiles create
  = fmap (transformRawTree . Tree.dirTree) .
      Tree.readDirectoryWith create

transformRawTree ::
                 Tree.DirTree a ->
                   MapTree.MapForest FilePath (Either Exception.IOException a)
transformRawTree root = transform [root]
  where transform = MapTree.MapForest . Map.fromList . fmap bind
        bind rawTree = (label, tree)
          where label = Tree.name rawTree
                tree
                  = case rawTree of
                        Tree.Failed{Tree.err = exception} -> MapTree.Leaf . Left $
                                                               exception
                        Tree.File{Tree.file = value} -> MapTree.Leaf $ Right value
                        Tree.Dir{Tree.contents = forest} -> MapTree.Node $ transform forest
