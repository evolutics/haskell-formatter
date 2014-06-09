module Evolutics.Tools.MyTraversable
       (Traversable, traverse, sequenceA) where
import qualified Control.Applicative as Applicative
import qualified Data.Array as Array
import qualified Data.Ix as Ix
import qualified Data.Traversable as Traversable

class (Functor t) => Traversable t where

        traverse ::
                   (Applicative.Applicative f) => (a -> f b) -> t a -> f (t b)
        traverse f = sequenceA . fmap f

        sequenceA :: (Applicative.Applicative f) => t (f a) -> f (t a)
        sequenceA = traverse id

instance Traversable Maybe where
        traverse = Traversable.traverse

instance Traversable [] where
        traverse = Traversable.traverse

instance (Ix.Ix i) => Traversable (Array.Array i) where
        traverse = Traversable.traverse
