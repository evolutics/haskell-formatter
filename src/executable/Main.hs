module Main (main) where
import qualified Control.Arrow as Arrow
import qualified Data.Function as Function
import qualified Options.Applicative as Applicative
import qualified System.Directory as Directory
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified Evolutics.Formatting as Formatting

data Arguments = Arguments{input :: Maybe FilePath,
                           output :: Maybe FilePath, force :: Bool}
               deriving (Eq, Ord, Show)

main :: IO ()
main = Applicative.execParser usage >>= externalFormat

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
           Applicative.long "input" Applicative.<> Applicative.metavar "FILE"
             Applicative.<>
             Applicative.help
               "Input source code file (default: standard input)")
      Applicative.<*>
      Applicative.optional
        (Applicative.strOption $
           Applicative.long "output" Applicative.<> Applicative.metavar "FILE"
             Applicative.<>
             Applicative.help
               "Output source code file (default: standard output)")
      Applicative.<*>
      Applicative.switch
        (Applicative.long forceLongName Applicative.<>
           Applicative.help
             "Allows the output file to overwrite the input file")

forceLongName :: String
forceLongName = "force"

externalFormat :: Arguments -> IO ()
externalFormat arguments
  = case (maybeInput, maybeOutput) of
        (Just inputPath, Just outputPath) -> if forceOverwriting then
                                               continue else
                                               do same <- sameExistentPaths inputPath outputPath
                                                  if same then exitWithError overwritingError else
                                                    continue
          where overwritingError
                  = concat
                      ["The output path ", show outputPath,
                       " would overwrite the input path ", show inputPath, ". ",
                       "Either use unequal paths or apply the ", show forceLongName,
                       " option."]
        _ -> continue
  where maybeInput = input arguments
        maybeOutput = output arguments
        forceOverwriting = force arguments
        continue = internalFormat maybeInput maybeOutput

exitWithError :: String -> IO ()
exitWithError message
  = do IO.hPutStrLn IO.stderr message
       Exit.exitFailure

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

internalFormat :: Maybe FilePath -> Maybe FilePath -> IO ()
internalFormat inputPath outputPath
  = transformUnlessError readInput transform writeOutput
  where readInput = maybe getContents readFile inputPath
        writeOutput = maybe putStr writeFile outputPath
        transform = Arrow.left show . Formatting.formatSource inputPath

transformUnlessError ::
                     IO String ->
                       (String -> Either String String) -> (String -> IO ()) -> IO ()
transformUnlessError readInput transform writeOutput
  = do string <- readInput
       case transform string of
           Left message -> exitWithError message
           Right string' -> writeOutput string'
