module Main (main) where
import qualified System.Environment as Environment
import qualified Evolutics.Formatting as Formatting
import qualified Evolutics.Tools as Tools

main :: IO ()
main
  = Environment.getArgs >>=
      Tools.transformFilesOrStandardStreams Formatting.formatSource
