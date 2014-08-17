module Main (main) where
import Prelude hiding (error)
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Language.Haskell.Formatter as Formatter
import qualified Options.Applicative as Applicative
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO

data Arguments = Arguments{input :: Maybe FilePath,
                           output :: Maybe FilePath, force :: Bool}
               deriving (Eq, Ord, Show)

main :: IO ()
main
  = do arguments <- Applicative.execParser usage
       maybeError <- externalFormat arguments
       Foldable.mapM_ exitWithError maybeError
  where exitWithError error
          = IO.hPutStrLn IO.stderr error >> Exit.exitFailure

usage :: Applicative.ParserInfo Arguments
usage
  = Applicative.info
      (Applicative.helper Applicative.<*> argumentParser)
      (Applicative.fullDesc Applicative.<>
         Applicative.header "A Haskell source code formatter")

argumentParser :: Applicative.Parser Arguments
argumentParser
  = Arguments Applicative.<$>
      Applicative.optional
        (Applicative.strOption $
           Applicative.long "input" Applicative.<> Applicative.short 'i'
             Applicative.<> Applicative.metavar "FILE"
             Applicative.<>
             Applicative.help
               "Input source code file (default: standard input)")
      Applicative.<*>
      Applicative.optional
        (Applicative.strOption $
           Applicative.long "output" Applicative.<> Applicative.short 'o'
             Applicative.<> Applicative.metavar "FILE"
             Applicative.<>
             Applicative.help
               "Output source code file (default: standard output)")
      Applicative.<*>
      Applicative.switch
        (Applicative.long "force" Applicative.<>
           Applicative.short forceStandardName
           Applicative.<>
           Applicative.help
             "Allows the output file to overwrite the input file")

forceStandardName :: Char
forceStandardName = 'f'

externalFormat :: Arguments -> IO (Maybe String)
externalFormat arguments
  = do maybeError <- checkArguments arguments
       case maybeError of
           Nothing -> internalFormat arguments
           Just error -> return $ Just error

checkArguments :: Arguments -> IO (Maybe String)
checkArguments arguments
  = case (maybeInput, maybeOutput) of
        (Just inputPath, Just outputPath) -> if forceOverwriting then
                                               return Nothing else
                                               do same <- sameExistentPaths inputPath outputPath
                                                  return $
                                                    if same then Just overwritingError else Nothing
          where overwritingError
                  = concat
                      ["The output path ", show outputPath,
                       " would overwrite the input path ", show inputPath, ". ",
                       "Either use unequal paths or apply the ", show forceStandardName,
                       " option."]
        _ -> return Nothing
  where maybeInput = input arguments
        maybeOutput = output arguments
        forceOverwriting = force arguments

sameExistentPaths :: FilePath -> FilePath -> IO Bool
sameExistentPaths left right
  = do exist <- bothPathsExist
       if exist then
         Function.on (Applicative.liftA2 FilePath.equalFilePath)
           Directory.canonicalizePath
           left
           right
         else return False
  where bothPathsExist
          = Function.on (Applicative.liftA2 (&&)) pathExists left right
        pathExists path
          = Applicative.liftA2 (||) (Directory.doesFileExist path) $
              Directory.doesDirectoryExist path

internalFormat :: Arguments -> IO (Maybe String)
internalFormat arguments
  = do inputString <- readInput
       case Formatter.format stream inputString of
           Left error -> return . Just $ prepareError error
           Right outputString -> writeOutput outputString >> return Nothing
  where readInput = maybe getContents readFile maybeInput
        maybeInput = input arguments
        stream
          = maybe Formatter.standardInput Formatter.createStreamName
              maybeInput
        writeOutput = maybe putStr writeFile maybeOutput
        maybeOutput = output arguments

prepareError :: Formatter.Error -> String
prepareError parseError@(Formatter.ParseError _ _)
  = show parseError
prepareError assertionError@(Formatter.AssertionError _)
  = unlines
      ["Oops, an error occurred.",
       "Feel free to report this, because it appears to be a bug. Thanks!",
       "The exact error message follows.", "", show assertionError]
