{-|
Description : Root of executable
-}
module Main (main) where
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter as Formatter
import qualified Options.Applicative as Applicative
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO

data Arguments = Arguments{input :: Maybe FilePath, output :: Maybe FilePath,
                           force :: Bool}
               deriving (Eq, Ord, Show)

main :: IO ()
main
  = do arguments <- Applicative.execParser utilityUsage
       maybeError <- externalFormat arguments
       Foldable.mapM_ exitWithError maybeError
  where exitWithError errorMessage
          = putErrorLine errorMessage >> Exit.exitFailure
        putErrorLine = IO.hPutStrLn IO.stderr

utilityUsage :: Applicative.ParserInfo Arguments
utilityUsage = Applicative.info parserWithHelp modifier
  where parserWithHelp = Applicative.helper Applicative.<*> argumentParser
        modifier
          = Monoid.mconcat
              [Applicative.fullDesc, oneLineDescription, description]
        oneLineDescription
          = Applicative.header
              "Haskell Formatter – A Haskell source code formatter"
        description
          = Applicative.progDesc $
              concat
                ["The Haskell Formatter formats Haskell source code ",
                 "in a strict way. The source code is read from INPUT, ",
                 "formatted, and written to OUTPUT."]

argumentParser :: Applicative.Parser Arguments
argumentParser
  = Arguments Applicative.<$> inputOption Applicative.<*> outputOption
      Applicative.<*> forceOption
  where inputOption
          = Applicative.optional . Applicative.strOption $
              Monoid.mconcat
                [Applicative.short 'i', Applicative.long "input",
                 Applicative.metavar "INPUT",
                 Applicative.help
                   "Input source code file (default: standard input)"]
        outputOption
          = Applicative.optional . Applicative.strOption $
              Monoid.mconcat
                [Applicative.short 'o', Applicative.long "output",
                 Applicative.metavar "OUTPUT",
                 Applicative.help
                   "Output source code file (default: standard output)"]
        forceOption
          = Applicative.switch $
              Monoid.mconcat
                [Applicative.short forceStandardName, Applicative.long "force",
                 Applicative.help
                   "Allows the output file to overwrite the input file"]

forceStandardName :: Char
forceStandardName = 'f'

externalFormat :: Arguments -> IO (Maybe String)
externalFormat arguments
  = do maybeError <- checkArguments arguments
       case maybeError of
           Nothing -> internalFormat arguments
           Just errorMessage -> return $ Just errorMessage

checkArguments :: Arguments -> IO (Maybe String)
checkArguments arguments
  = case (maybeInput, maybeOutput) of
        (Just inputPath, Just outputPath) -> if forceOverwriting then
                                               return Nothing else
                                               do same <- sameExistentPaths
                                                            inputPath
                                                            outputPath
                                                  return $
                                                    if same then
                                                      Just overwritingError else
                                                      Nothing
          where overwritingError
                  = concat
                      ["The output path ", show outputPath,
                       " would overwrite the input path ", show inputPath, ". ",
                       "Either use unequal paths or apply the ",
                       show forceStandardName, " option, please."]
        _ -> return Nothing
  where maybeInput = input arguments
        maybeOutput = output arguments
        forceOverwriting = force arguments

{-| Do both file paths exist and refer to the same file or folder? -}
sameExistentPaths :: FilePath -> FilePath -> IO Bool
sameExistentPaths left right
  = do exist <- bothPathsExist
       if exist then
         liftedOn FilePath.equalFilePath Directory.canonicalizePath left right
         else return False
  where bothPathsExist = liftedOn (&&) pathExists left right
        liftedOn = Function.on . Applicative.liftA2
        pathExists path
          = Applicative.liftA2 (||) (Directory.doesFileExist path) $
              Directory.doesDirectoryExist path

internalFormat :: Arguments -> IO (Maybe String)
internalFormat arguments
  = do inputString <- readInput
       case Formatter.format stream inputString of
           Left libraryError -> return . Just $ showError libraryError
           Right outputString -> writeOutput outputString >> return Nothing
  where readInput = maybe getContents readFile maybeInput
        maybeInput = input arguments
        stream
          = maybe Formatter.standardInput Formatter.createStreamName maybeInput
        writeOutput = maybe putStr writeFile maybeOutput
        maybeOutput = output arguments

showError :: Formatter.Error -> String
showError libraryError
  = if Formatter.isAssertionError libraryError then assertionError else rawError
  where assertionError
          = unlines
              [concat
                 ["Oops, an error occurred. ", "Feel free to report this, ",
                  "because it appears to be a bug. Thanks! ",
                  "The specific error message follows."],
               "", rawError]
        rawError = show libraryError
