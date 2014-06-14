module Evolutics.Tools.Newlines (splitSeparatedLines) where
import qualified Evolutics.Tools.Splitting as Splitting

splitSeparatedLines :: String -> [String]
splitSeparatedLines = Splitting.separate newlines

newlines :: [String]
newlines
  = ["\r\n", "\n", "\v", "\f", "\r", "\133", "\8232", "\8233"]
