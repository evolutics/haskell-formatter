module Main (main) where
import qualified System.Environment as Environment
import qualified Evolutics.Formatting as Formatting
import qualified Evolutics.Tools.FileStreams as FileStreams

main :: IO ()
main
  = Environment.getArgs >>=
      FileStreams.transformFilesOrStandardStreams Formatting.formatSource
