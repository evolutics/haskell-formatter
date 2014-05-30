module Main (main) where
import qualified Data.List as List
import qualified Language.Haskell.Exts as Exts
import qualified Language.Haskell.Parser as Parser
import qualified Language.Haskell.Pretty as Pretty
import qualified Language.Haskell.Syntax as Syntax
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
  = format . Parser.parseModuleWithMode parseMode
  where format (Parser.ParseFailed location message)
          = Left $ showSourceLocation location message
        format (Parser.ParseOk syntaxTree)
          = Right $ Pretty.prettyPrint syntaxTree
        parseMode
          = maybe Parser.defaultParseMode Parser.ParseMode maybeSourceName
 
showSourceLocation :: Syntax.SrcLoc -> String -> String
showSourceLocation location message
  = showLocation location ++ majorSeparator ++ message
  where showLocation (Syntax.SrcLoc filename line column)
          = List.intercalate minorSeparator $ filename :
              map show [line, column]
        minorSeparator = ":"
        majorSeparator = ": "
