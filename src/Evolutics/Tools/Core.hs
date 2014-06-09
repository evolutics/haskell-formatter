module Evolutics.Tools.Core (transformFilesOrStandardStreams) where
import qualified System.IO as IO

transformFilesOrStandardStreams ::
                                (Maybe FilePath -> String -> Either String String) ->
                                  [FilePath] -> IO ()
transformFilesOrStandardStreams transform []
  = transformFileUnlessFailure getContents (transform Nothing) putStr
transformFilesOrStandardStreams transform files
  = mapM_ transform' files
  where transform' file
          = transformFileUnlessFailure (readFile file)
              (transform $ Just file)
              $ writeFile file

transformFileUnlessFailure ::
                           IO String ->
                             (String -> Either String String) -> (String -> IO ()) -> IO ()
transformFileUnlessFailure read transform write
  = do string <- read
       case transform string of
           Left message -> IO.hPutStrLn IO.stderr message
           Right string' -> write string'
