module Evolutics.Tools.MyTraversable
       (Traversable, traverse, sequenceA) where
import qualified Control.Applicative as Applicative

class (Functor t) => Traversable t where

        traverse ::
                   (Applicative.Applicative f) => (a -> f b) -> t a -> f (t b)
        traverse f = sequenceA . fmap f

        sequenceA :: (Applicative.Applicative f) => t (f a) -> f (t a)
        sequenceA = traverse id
