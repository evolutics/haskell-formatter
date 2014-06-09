module Evolutics.Tools.Core
       (transformFilesOrStandardStreams, formatSourceMessage,
        comparePortions)
       where
import qualified Data.Function as Function
import qualified Data.List as List
import qualified System.IO as IO
import qualified Language.Haskell.Exts.Annotated as Exts

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

formatSourceMessage :: Exts.SrcLoc -> String -> String
formatSourceMessage location message
  = formatSourceLocation location ++ separator ++ message
  where separator = ": "

formatSourceLocation :: Exts.SrcLoc -> String
formatSourceLocation (Exts.SrcLoc file line column)
  = List.intercalate separator $ file : map show [line, column]
  where separator = ":"

comparePortions :: Exts.SrcSpan -> Exts.SrcSpan -> Ordering
comparePortions left right
  = if Function.on (==) Exts.srcSpanFilename left right then
      compareIgnoringFile else EQ
  where compareIgnoringFile
          | Exts.srcSpanEnd left < Exts.srcSpanStart right = LT
          | Exts.srcSpanStart left > Exts.srcSpanEnd right = GT
          | otherwise = EQ
