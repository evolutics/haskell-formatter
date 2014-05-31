module Evolutics.Tools (showSourceLocation) where
import qualified Data.List as List
import qualified Language.Haskell.Exts.Annotated as Exts

showSourceLocation :: Exts.SrcLoc -> String -> String
showSourceLocation location message
  = showLocation location ++ majorSeparator ++ message
  where showLocation (Exts.SrcLoc filename line column)
          = List.intercalate minorSeparator $
              filename : map show [line, column]
        minorSeparator = ":"
        majorSeparator = ": "
