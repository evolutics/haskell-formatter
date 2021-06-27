{-|
Description : Trees with unique labels
-}
module Language.Haskell.Formatter.Internal.MapTree
       (MapTree(..), MapForest, isEmpty, summarizeLeaves, indentTree) where
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Internal.Newline as Newline

data MapTree k a = Leaf a
                 | Node (MapForest k a)
                     deriving (Eq, Ord, Show)

type MapForest k a = Map.Map k (MapTree k a)

instance Functor (MapTree k) where
        fmap function (Leaf value) = Leaf $ function value
        fmap function (Node forest) = Node $ fmap (fmap function) forest

isEmpty :: MapTree k a -> Bool
isEmpty (Leaf _) = False
isEmpty (Node forest) = Map.null forest

summarizeLeaves ::
                  (Ord k, Monoid.Monoid b) =>
                  MapForest k (Either a b) -> MapTree k (Either a (Map.Map k b))
summarizeLeaves = summarize Map.empty
  where summarize labels root
          = if Map.null lefts then
              if Map.null forests then Leaf . Right $ labels' else
                fromMap (summarize labels') forests
              else fromMap (Leaf . Left) lefts
          where (lefts, rights) = Map.mapEither id values
                (values, forests) = Map.mapEither distinguish root
                distinguish (Leaf value) = Left value
                distinguish (Node forest) = Right forest
                labels' = Map.unionWith Monoid.mappend labels rights
                fromMap function = Node . fmap function

indentTree :: MapTree String String -> String
indentTree = Newline.joinSeparatedLines . indentLines
  where indentLines (Leaf value) = Newline.splitSeparatedLines value
        indentLines (Node forest) = foldMapWithKey indentBinding forest
        foldMapWithKey create = uncurry create Monad.<=< Map.toAscList
        indentBinding label tree = Monoid.mappend labelLines treeLines
          where labelLines = Newline.splitSeparatedLines label
                treeLines = indent Applicative.<$> indentLines tree
        indent = (indentation ++)
        indentation = "    "
