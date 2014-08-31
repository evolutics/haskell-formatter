{-|
Description : Container utilities
-}
module Language.Haskell.Formatter.Toolkit.Visit
       (findJust, halfZipWith, mapAccumulateLeftWithCreation) where
import qualified Data.Foldable as Foldable
import qualified Data.Maybe as Maybe
import qualified Data.Traversable as Traversable

{-| @findJust f c@ returns the first non-'Nothing' value of @c@ mapped with @f@,
    or 'Nothing' if there is none. -}
findJust :: (Functor t, Foldable.Foldable t) => (a -> Maybe b) -> t a -> Maybe b
findJust function = Foldable.asum . fmap function

{-| @halfZipWith m b e@ zips the elements of @b@ and @e@ with @m@, using the
    structure of @b@. 'Nothing' is returned if and only if @b@ does not have
    enough elements. -}
halfZipWith ::
              (Traversable.Traversable t, Foldable.Foldable f) =>
              (a -> b -> c) -> t a -> f b -> Maybe (t c)
halfZipWith merge base extension = Traversable.sequenceA zippedMaybe
  where (_, zippedMaybe) = Traversable.mapAccumL process extensionList base
        process [] _ = ([], Nothing)
        process (extensionElement : list) baseElement
          = (list, Just $ merge baseElement extensionElement)
        extensionList = Foldable.toList extension

{-| Like 'Traversable.mapAccumL', but with a function to create the base. -}
mapAccumulateLeftWithCreation ::
                                (Traversable.Traversable t) =>
                                (a -> b -> (a, c)) ->
                                  (b -> a) -> t b -> (Maybe a, t c)
mapAccumulateLeftWithCreation process createBase
  = Traversable.mapAccumL processMaybe Nothing
  where processMaybe maybeBefore element = (Just after, element')
          where (after, element') = process before element
                before = Maybe.fromMaybe (createBase element) maybeBefore
