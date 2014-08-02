module Language.Haskell.Formatter.Toolkit.Newline
       (splitSeparatedLines) where
import qualified Language.Haskell.Formatter.Toolkit.Splitting
       as Splitting

splitSeparatedLines :: String -> [String]
splitSeparatedLines = Splitting.separate newlines

newlines :: [String]
newlines
  = ["\r\n", "\n", "\v", "\f", "\r", "\133", "\8232", "\8233"]
