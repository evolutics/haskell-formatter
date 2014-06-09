module Main (main) where
import qualified System.Environment as Environment
import qualified Evolutics.Formatting as Formatting
import qualified Evolutics.Tools.Core as Core

main :: IO ()
main
  = Environment.getArgs >>=
      Core.transformFilesOrStandardStreams Formatting.formatSource
