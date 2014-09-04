{-|
Description : Root of executable
-}
module Main (main) where
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter as Formatter
import qualified Language.Haskell.Formatter.Internal.Newline as Newline
import qualified Language.Haskell.Formatter.Internal.StyleFileFormat
       as StyleFileFormat
import qualified Language.Haskell.Formatter.Internal.TreeFormat as TreeFormat
import qualified Options.Applicative as Applicative
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO

data Arguments = Arguments{input :: Maybe FilePath, output :: Maybe FilePath,
                           force :: Bool, styleFile :: Maybe FilePath}
               deriving (Eq, Ord, Show)

main :: IO ()
main
  = do arguments <- Applicative.execParser utilityUsage
       maybeError <- formatWithUncheckedArguments arguments
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
              createParagraphs
                [["The Haskell Formatter formats Haskell source code. ",
                  "It is strict in that it fundamentally rearranges code."],
                 ["The source code is read from INPUT, ",
                  "formatted, and written to OUTPUT."]]

argumentParser :: Applicative.Parser Arguments
argumentParser
  = Arguments Applicative.<$> inputOption Applicative.<*> outputOption
      Applicative.<*> forceOption
      Applicative.<*> styleOption
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
        styleOption
          = Applicative.optional . Applicative.strOption $
              Monoid.mconcat
                [Applicative.short 's', Applicative.long "style",
                 Applicative.metavar "STYLE",
                 Applicative.help "Formatting style file"]

forceStandardName :: Char
forceStandardName = 'f'

createParagraphs :: [[String]] -> String
createParagraphs
  = Newline.joinSeparatedLines . List.intersperse emptyLine . fmap concat
  where emptyLine = ""

formatWithUncheckedArguments :: Arguments -> IO (Maybe String)
formatWithUncheckedArguments arguments
  = do maybeError <- checkArguments arguments
       case maybeError of
           Nothing -> formatWithCheckedArguments arguments
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

formatWithCheckedArguments :: Arguments -> IO (Maybe String)
formatWithCheckedArguments arguments
  = do maybeConfiguration <- getConfiguration arguments
       case maybeConfiguration of
           Left errorMessage -> return $ Just errorMessage
           Right configuration -> formatWithConfiguration arguments
                                    configuration

getConfiguration :: Arguments -> IO (Either String Formatter.Configuration)
getConfiguration arguments
  = do maybeStyle <- getStyle arguments
       return $
         case maybeStyle of
             Left message -> Left message
             Right style -> Right configuration
               where configuration
                       = defaults{Formatter.configurationStyle = style}
  where defaults
          = Formatter.defaultConfiguration{Formatter.configurationStreamName =
                                             stream}
        stream
          = maybe Formatter.standardInput Formatter.createStreamName maybeInput
        maybeInput = input arguments

getStyle :: Arguments -> IO (Either String Formatter.Style)
getStyle arguments
  = case styleFile arguments of
        Nothing -> return $ Right defaultStyle
        Just file -> TreeFormat.parseYamlFile StyleFileFormat.treeFormat
                       defaultStyle
                       file
  where defaultStyle
          = Formatter.configurationStyle Formatter.defaultConfiguration

formatWithConfiguration ::
                        Arguments ->
                          Formatter.Configuration -> IO (Maybe String)
formatWithConfiguration arguments configuration
  = do inputString <- readInput
       case Formatter.format configuration inputString of
           Left libraryError -> return . Just $ showError libraryError
           Right outputString -> writeOutput outputString >> return Nothing
  where readInput = maybe getContents readFile maybeInput
        maybeInput = input arguments
        writeOutput = maybe putStr writeFile maybeOutput
        maybeOutput = output arguments

showError :: Formatter.Error -> String
showError libraryError
  = if Formatter.isAssertionError libraryError then assertionError else rawError
  where assertionError
          = createParagraphs
              [["Oops, an error occurred. ", "Feel free to report this, ",
                "because it appears to be a bug. Thanks! ",
                "The specific error message follows."],
               [rawError]]
        rawError = show libraryError
