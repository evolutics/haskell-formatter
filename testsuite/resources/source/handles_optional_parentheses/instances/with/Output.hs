
class Sized a where

        size :: a -> Int

instance (Sized a) => Sized [a] where
        size = sum . fmap size
