module Evolutics.Tools.Functions (doubleArgument) where

doubleArgument :: (a -> a -> b) -> a -> b
doubleArgument function argument = function argument argument
