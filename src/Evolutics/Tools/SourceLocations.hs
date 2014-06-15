module Evolutics.Tools.SourceLocations
       (Portioned, portion, formatMessage, comparePortions) where
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Language.Haskell.Exts.Annotated as Exts

class Portioned a where

        portion :: a -> Exts.SrcSpan

instance Portioned Exts.SrcSpanInfo where
        portion = Exts.srcInfoSpan

instance Portioned Exts.Comment where
        portion (Exts.Comment _ portion _) = portion

formatMessage :: Exts.SrcLoc -> String -> String
formatMessage position message
  = formatPosition position ++ separator ++ message
  where separator = ": "

formatPosition :: Exts.SrcLoc -> String
formatPosition (Exts.SrcLoc file line column)
  = List.intercalate separator $ file : map show [line, column]
  where separator = ":"

comparePortions :: (Portioned a, Portioned b) => a -> b -> Ordering
comparePortions leftPortion rightPortion
  = if Function.on (==) Exts.srcSpanFilename left right then
      compareIgnoringFile else EQ
  where left = portion leftPortion
        right = portion rightPortion
        compareIgnoringFile
          | Exts.srcSpanEnd left < Exts.srcSpanStart right = LT
          | Exts.srcSpanStart left > Exts.srcSpanEnd right = GT
          | otherwise = EQ
