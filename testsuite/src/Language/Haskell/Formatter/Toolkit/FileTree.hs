{-|
Description : Creating trees from files and folders
-}
module Language.Haskell.Formatter.Toolkit.FileTree (collectFiles) where
import qualified Control.Applicative as Applicative
import qualified Control.Exception as Exception
import qualified Data.Map.Strict as Map
import qualified Language.Haskell.Formatter.Internal.MapTree as MapTree
import qualified System.Directory.Tree as Tree

collectFiles ::
             (FilePath -> IO a) ->
               FilePath ->
                 IO
                   (MapTree.MapForest FilePath (Either Exception.IOException a))
collectFiles create rootFolder
  = do rawTree <- Tree.dirTree Applicative.<$>
                    Tree.readDirectoryWith create rootFolder
       return $ transformRawTree rawTree

transformRawTree ::
                 Tree.DirTree a ->
                   MapTree.MapForest FilePath (Either Exception.IOException a)
transformRawTree root = transform [root]
  where transform = MapTree.MapForest . Map.fromList . fmap bind
        bind rawTree = (label, tree)
          where label = Tree.name rawTree
                tree
                  = case rawTree of
                        Tree.Failed{Tree.err = exception} -> MapTree.Leaf . Left
                                                               $ exception
                        Tree.File{Tree.file = value} -> MapTree.Leaf $
                                                          Right value
                        Tree.Dir{Tree.contents = forest} -> MapTree.Node $
                                                              transform forest
