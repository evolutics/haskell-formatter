module Main (main) where
import qualified Data.List as List
import qualified Language.Haskell.Exts.Annotated as Annotated
import qualified System.Environment as Environment
import qualified System.IO as IO

main :: IO ()
main
  = do programArguments <- Environment.getArgs
       case programArguments of
           [] -> readFormatWrite getContents Nothing putStrLn
           filenames -> mapM_ formatFile filenames
             where formatFile filename
                     = readFormatWrite (readFile filename) (Just filename)
                         (writeFile filename)

readFormatWrite ::
                IO String -> Maybe String -> (String -> IO ()) -> IO ()
readFormatWrite read maybeSourceName write
  = do source <- read
       let formattedSource = formatSource maybeSourceName source in
         either (IO.hPutStrLn IO.stderr) write formattedSource

formatSource :: Maybe String -> String -> Either String String
formatSource maybeSourceName
  = checkToFormat . Annotated.parseFileContentsWithComments parseMode
  where checkToFormat (Annotated.ParseFailed location message)
          = Left $ showSourceLocation location message
        checkToFormat (Annotated.ParseOk parseTree)
          = Right . show . format $ parseToSourceTree parseTree
        parseMode
          = case maybeSourceName of
                Nothing -> Annotated.defaultParseMode
                Just
                  sourceName -> Annotated.defaultParseMode{Annotated.parseFilename =
                                                             sourceName}

showSourceLocation :: Annotated.SrcLoc -> String -> String
showSourceLocation location message
  = showLocation location ++ majorSeparator ++ message
  where showLocation (Annotated.SrcLoc filename line column)
          = List.intercalate minorSeparator $
              filename : map show [line, column]
        minorSeparator = ":"
        majorSeparator = ": "

format :: SourceTree -> SourceTree
format SourceTree{sourceElement = element}
  = SourceTree{sourceElement = formattedElement,
               sourceComments = formattedComments}
  where formattedElement
          = Annotated.fromParseResult . Annotated.parseFileContents $
              Annotated.prettyPrint element
        formattedComments = []

data SourceTree = SourceTree{sourceElement ::
                             Annotated.Module Annotated.SrcSpanInfo,
                             sourceComments :: [Annotated.Comment]}

instance Show SourceTree where
        show SourceTree{sourceElement = element, sourceComments = comments}
          = Annotated.exactPrint element comments

parseToSourceTree ::
                  (Annotated.Module Annotated.SrcSpanInfo, [Annotated.Comment]) ->
                    SourceTree
parseToSourceTree (element, comments)
  = SourceTree{sourceElement = element, sourceComments = comments}
