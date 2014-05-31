module Main (main) where
import qualified Data.List as List
import qualified Language.Haskell.Exts.Annotated as Exts
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
  = checkToFormat . Exts.parseFileContentsWithComments parseMode
  where checkToFormat (Exts.ParseFailed location message)
          = Left $ showSourceLocation location message
        checkToFormat (Exts.ParseOk parseTree)
          = Right . show . format $ parseToSourceTree parseTree
        parseMode
          = case maybeSourceName of
                Nothing -> Exts.defaultParseMode
                Just sourceName -> Exts.defaultParseMode{Exts.parseFilename =
                                                           sourceName}

showSourceLocation :: Exts.SrcLoc -> String -> String
showSourceLocation location message
  = showLocation location ++ majorSeparator ++ message
  where showLocation (Exts.SrcLoc filename line column)
          = List.intercalate minorSeparator $
              filename : map show [line, column]
        minorSeparator = ":"
        majorSeparator = ": "

format :: SourceTree -> SourceTree
format SourceTree{sourceElement = element}
  = SourceTree{sourceElement = formattedElement,
               sourceComments = formattedComments}
  where formattedElement
          = Exts.fromParseResult . Exts.parseFileContents $
              Exts.prettyPrint element
        formattedComments = []

data SourceTree = SourceTree{sourceElement ::
                             Exts.Module Exts.SrcSpanInfo,
                             sourceComments :: [Exts.Comment]}

instance Show SourceTree where
        show SourceTree{sourceElement = element, sourceComments = comments}
          = Exts.exactPrint element comments

parseToSourceTree ::
                  (Exts.Module Exts.SrcSpanInfo, [Exts.Comment]) -> SourceTree
parseToSourceTree (element, comments)
  = SourceTree{sourceElement = element, sourceComments = comments}
