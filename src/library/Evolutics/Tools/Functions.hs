module Evolutics.Tools.Functions
       (untilRight, iterateUntilNothing, findJust, halfZipWith,
        mapAccumulateLeft1)
       where
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Traversable

untilRight :: (a -> Either a b) -> a -> b
untilRight function base
  = case function base of
        Left intermediate -> untilRight function intermediate
        Right final -> final

iterateUntilNothing :: (a -> Maybe a) -> a -> [a]
iterateUntilNothing function base
  = base :
      case function base of
          Nothing -> []
          Just next -> iterateUntilNothing function next

findJust ::
           (Functor t, Foldable.Foldable t) =>
           (a -> Maybe b) -> t a -> Maybe b
findJust function = Monad.join . findMap Maybe.isJust function

findMap ::
          (Functor t, Foldable.Foldable t) =>
          (b -> Bool) -> (a -> b) -> t a -> Maybe b
findMap predicate function
  = Foldable.find predicate . fmap function

halfZipWith ::
              (Traversable.Traversable t, Foldable.Foldable f) =>
              (a -> b -> c) -> t a -> f b -> Maybe (t c)
halfZipWith merge base extension
  = Traversable.sequenceA zippedMaybe
  where (_, zippedMaybe)
          = Traversable.mapAccumL process extensionList base
        process [] _ = ([], Nothing)
        process (extensionElement : list) baseElement
          = (list, Just $ merge baseElement extensionElement)
        extensionList = Foldable.toList extension

mapAccumulateLeft1 ::
                     (Traversable.Traversable t) =>
                     (a -> b -> (a, b)) -> (b -> a) -> t b -> (Maybe a, t b)
mapAccumulateLeft1 process createBase
  = Traversable.mapAccumL function base
  where function maybeBefore element = (Just after, element')
          where (after, element') = process before element
                before = Maybe.fromMaybe (createBase element) maybeBefore
        base = Nothing
