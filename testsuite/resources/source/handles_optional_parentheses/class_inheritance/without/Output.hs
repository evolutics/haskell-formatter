import Data.Monoid

class Monoid a => Group a where

        inverse :: a -> a
