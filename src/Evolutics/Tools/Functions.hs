module Evolutics.Tools.Functions
       (doubleArgument, iterateUntil, findJust) where
import qualified Control.Monad as Monad
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe

doubleArgument :: (a -> a -> b) -> a -> b
doubleArgument function argument = function argument argument

iterateUntil :: (a -> Maybe a) -> a -> [a]
iterateUntil function base
  = base :
      case function base of
          Nothing -> []
          Just next -> iterateUntil function next

findJust ::
           (Functor t, Foldable.Foldable t) =>
           (a -> Maybe b) -> t a -> Maybe b
findJust function = Monad.join . findMap Maybe.isJust function

findMap ::
          (Functor t, Foldable.Foldable t) =>
          (b -> Bool) -> (a -> b) -> t a -> Maybe b
findMap predicate function
  = Foldable.find predicate . fmap function
