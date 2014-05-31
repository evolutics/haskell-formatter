module Main (main) where
import qualified System.Environment as Environment
import qualified System.IO as IO
import qualified Evolutics.Formatter as Formatter

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
                IO String -> Maybe FilePath -> (String -> IO ()) -> IO ()
readFormatWrite read maybeFilename write
  = do source <- read
       let formattedSource = Formatter.formatSource maybeFilename source
         in either (IO.hPutStrLn IO.stderr) write formattedSource
