module Evolutics.Tools
       (transformFilesOrStandardStreams, showSourceLocation) where
import qualified Data.List as List
import qualified System.IO as IO
import qualified Language.Haskell.Exts.Annotated as Exts

transformFilesOrStandardStreams ::
                                (Maybe FilePath -> String -> Either String String) ->
                                  [FilePath] -> IO ()
transformFilesOrStandardStreams transform []
  = transformFileUnlessFailure getContents (transform Nothing)
      putStrLn
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

showSourceLocation :: Exts.SrcLoc -> String -> String
showSourceLocation location message
  = showLocation location ++ majorSeparator ++ message
  where showLocation (Exts.SrcLoc file line column)
          = List.intercalate minorSeparator $ file : map show [line, column]
        minorSeparator = ":"
        majorSeparator = ": "
