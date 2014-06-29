module Evolutics.Tools.DirectoryData (Exception, createTree) where
import qualified Control.Exception as Exception
import qualified Data.Foldable as Foldable
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Set as Set
import qualified System.Directory.Tree as Tree
import qualified Evolutics.Tools.MapTree as MapTree

data Exception = InputOutputException Exception.IOException
               | MissingKey FilePath
               | UnexpectedKey FilePath

instance Show Exception where
        show (InputOutputException exception) = show exception
        show (MissingKey key) = "Missing key " ++ show key
        show (UnexpectedKey key) = "Unexpected key " ++ show key

createTree ::
             (Monoid.Monoid a) =>
             (FilePath -> IO a) ->
               Set.Set FilePath ->
                 FilePath ->
                   IO (MapTree.MapTree FilePath ([Exception], Map.Map FilePath a))
createTree create keys
  = fmap (createLeafMaps keys) . collectFiles create

createLeafMaps ::
                 (Monoid.Monoid a) =>
                 Set.Set FilePath ->
                   MapTree.MapTree FilePath (Either Exception.IOException a) ->
                     MapTree.MapTree FilePath ([Exception], Map.Map FilePath a)
createLeafMaps expectedKeys
  = MapTree.summarizeLeaves create . fmap prepare
  where create filesMap = (exceptions, actualMap)
          where exceptions = originalExceptions ++ keyExceptions
                originalExceptions
                  = Foldable.concatMap (map InputOutputException . fst) filesMap
                keyExceptions = checkKeys expectedKeys actualKeys
                actualKeys = Map.keysSet actualMap
                actualMap = fmap snd filesMap
        prepare (Left exception) = ([exception], Monoid.mempty)
        prepare (Right value) = (Monoid.mempty, value)

checkKeys :: Set.Set FilePath -> Set.Set FilePath -> [Exception]
checkKeys expectedKeys actualKeys
  = missingExceptions ++ unexpectedExceptions
  where missingExceptions = mapSetOrdered MissingKey missingKeys
        mapSetOrdered function = map function . Set.elems
        missingKeys = Set.difference expectedKeys actualKeys
        unexpectedExceptions = mapSetOrdered UnexpectedKey unexpectedKeys
        unexpectedKeys = Set.difference actualKeys expectedKeys

collectFiles ::
             (FilePath -> IO a) ->
               FilePath ->
                 IO (MapTree.MapTree FilePath (Either Exception.IOException a))
collectFiles create
  = fmap (transformRawTree . Tree.dirTree) .
      Tree.readDirectoryWith create

transformRawTree ::
                 Tree.DirTree a ->
                   MapTree.MapTree FilePath (Either Exception.IOException a)
transformRawTree root = MapTree.Node $ transformForest [root]
  where transformForest = Map.fromList . map bind
        bind tree = (label, children)
          where label = Tree.name tree
                children
                  = case tree of
                        Tree.Failed{Tree.err = exception} -> MapTree.Leaf . Left $
                                                               exception
                        Tree.File{Tree.file = value} -> MapTree.Leaf $ Right value
                        Tree.Dir{Tree.contents = forest} -> MapTree.Node $
                                                              transformForest forest
