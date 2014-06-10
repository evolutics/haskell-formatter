module Evolutics.Tools.HalfZippable (halfZipWith) where
import qualified Data.Foldable as Foldable
import qualified Data.Traversable as Traversable

halfZipWith ::
              (Traversable.Traversable t) => (a -> b -> c) -> t a -> t b -> t c
halfZipWith merge base extension
  = snd $ Traversable.mapAccumL process extensionList base
  where process (extensionElement : list) baseElement
          = (list, merge baseElement extensionElement)
        extensionList = Foldable.toList extension
