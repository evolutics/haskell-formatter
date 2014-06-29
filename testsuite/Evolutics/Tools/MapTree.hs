module Evolutics.Tools.MapTree (MapTree(..), summarizeLeaves) where
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid

data MapTree k a = Leaf a
                 | Node (Map.Map k (MapTree k a))

instance Functor (MapTree k) where
        fmap function (Leaf value) = Leaf $ function value
        fmap function (Node children)
          = Node $ fmap (fmap function) children

summarizeLeaves ::
                  (Ord k, Monoid.Monoid a) =>
                  (Map.Map k a -> b) -> MapTree k a -> MapTree k b
summarizeLeaves create (Node root) = transform Map.empty root
  where transform labels forest
          = if Map.null nodes then Leaf $ create labels' else
              Node $ fmap (transform labels') nodes
          where (leaves, nodes) = Map.mapEither distinguish forest
                distinguish (Leaf value) = Left value
                distinguish (Node children) = Right children
                labels' = Map.unionWith Monoid.mappend labels leaves
