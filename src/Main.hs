module Main (main) where
import qualified System.Environment as Environment
import qualified Evolutics.Formatter as Formatter
import qualified Evolutics.Tools as Tools

main :: IO ()
main
  = Environment.getArgs >>=
      Tools.transformFilesOrStandardStreams Formatter.formatSource
