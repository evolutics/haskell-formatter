module Evolutics.Tools.MapTree
       (MapTree(..), MapForest(..), summarizeLeaves) where
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid

data MapTree k a = Leaf a
                 | Node (MapForest k a)

data MapForest k a = MapForest (Map.Map k (MapTree k a))

instance Functor (MapTree k) where
        fmap function (Leaf value) = Leaf $ function value
        fmap function (Node forest) = Node $ fmap function forest

instance Functor (MapForest k) where
        fmap function = mapRoots (fmap function)

mapRoots ::
         (MapTree k a -> MapTree k b) -> MapForest k a -> MapForest k b
mapRoots function (MapForest children)
  = MapForest $ fmap function children

summarizeLeaves ::
                  (Ord k, Monoid.Monoid b) =>
                  MapForest k (Either a b) -> MapTree k (Either a (Map.Map k b))
summarizeLeaves = summarize Map.empty
  where summarize labels (MapForest children)
          = if Map.null lefts then
              if Map.null forests then Leaf . Right $ labels' else
                fromMap (summarize labels') forests
              else fromMap (Leaf . Left) lefts
          where (lefts, rights) = Map.mapEither id values
                (values, forests) = Map.mapEither distinguish children
                distinguish (Leaf value) = Left value
                distinguish (Node forest) = Right forest
                labels' = Map.unionWith Monoid.mappend labels rights

fromMap :: (a -> MapTree k b) -> Map.Map k a -> MapTree k b
fromMap function = Node . MapForest . fmap function
