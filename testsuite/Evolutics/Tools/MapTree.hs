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
        fmap function (MapForest children)
          = MapForest $ fmap (fmap function) children

summarizeLeaves ::
                  (Ord k, Monoid.Monoid a) =>
                  (Map.Map k a -> b) -> MapForest k a -> MapTree k b
summarizeLeaves create = summarize Map.empty
  where summarize labels (MapForest children)
          = if Map.null forests then Leaf $ create labels' else
              Node . MapForest $ fmap (summarize labels') forests
          where (values, forests) = Map.mapEither distinguish children
                distinguish (Leaf value) = Left value
                distinguish (Node forest) = Right forest
                labels' = Map.unionWith Monoid.mappend labels values
