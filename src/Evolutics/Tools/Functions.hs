module Evolutics.Tools.Functions
       (doubleArgument, untilRight, iterateUntilNothing, findJust,
        halfZipWith)
       where
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Traversable

doubleArgument :: (a -> a -> b) -> a -> b
doubleArgument function argument = function argument argument

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
              (Traversable.Traversable t) => (a -> b -> c) -> t a -> t b -> t c
halfZipWith merge base extension
  = snd $ Traversable.mapAccumL process extensionList base
  where process (extensionElement : list) baseElement
          = (list, merge baseElement extensionElement)
        extensionList = Foldable.toList extension
