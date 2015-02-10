import qualified Control.Monad as Monad
import qualified Control.Monad.Loops as Loops
import qualified Data.Monoid as Monoid
import qualified System.Environment as Environment
import qualified System.Exit as Exit
import qualified System.FilePath as FilePath
import qualified System.IO as IO
import qualified System.IO.Strict as Strict
import qualified System.Process as Process
import qualified Text.Regex as Regex

type PackageName = String

type PackageVersion = String

main :: IO ()
main
  = do arguments <- Environment.getArgs
       success <- case arguments of
                      [cabalFile] -> checkedMain cabalFile
                      _ -> return False
       Monad.unless success $
         do IO.hPutStrLn IO.stderr errorMessage
            Exit.exitFailure
  where errorMessage = "An error occurred."

checkedMain :: FilePath -> IO Bool
checkedMain cabalFile
  = do IO.hSetBuffering IO.stdout IO.NoBuffering
       success <- Loops.andM
                    [call "git" ["clone", rootFolder, "."],
                     call "make" ["clean"]]
       Monad.when success $
         do cabalTemplate <- Strict.readFile adaptedCabalFile
            let adaptCabalFile packageName
                  = writeFile adaptedCabalFile .
                      composeCabalFile cabalTemplate packageName
            checkDependencies adaptCabalFile
       return success
  where (rootFolder, adaptedCabalFile) = FilePath.splitFileName cabalFile

call :: FilePath -> [String] -> IO Bool
call utility arguments = rawCall process'
  where process'
          = process{Process.std_in = Process.CreatePipe,
                    Process.std_out = Process.CreatePipe}
        process = Process.proc utility arguments

rawCall :: Process.CreateProcess -> IO Bool
rawCall process
  = do (_, _, _, handle) <- Process.createProcess process
       exit <- Process.waitForProcess handle
       return $ exit == Exit.ExitSuccess

composeCabalFile :: String -> PackageName -> PackageVersion -> String
composeCabalFile template packageName packageVersion
  = Regex.subRegex searchPattern template replacementPattern
  where searchPattern
          = Regex.mkRegex $ concat ["( ", packageName, ") [^,]+(,|$)"]
        replacementPattern = concat ["\\1 == ", packageVersion, "\\2"]

checkDependencies :: (PackageName -> PackageVersion -> IO ()) -> IO ()
checkDependencies adaptCabalFile
  = do packageName <- getLine
       putStrLn packageName
       Monad.unless (null packageName) $
         do checkPackage $ adaptCabalFile packageName
            putStrLn ""
            checkDependencies adaptCabalFile

checkPackage :: (PackageVersion -> IO ()) -> IO ()
checkPackage adaptCabalFile
  = do packageVersions <- fmap (reverse . words) getLine
       untilWhile checkVersion packageVersions
  where checkVersion packageVersion
          = do putStr $ Monoid.mappend packageVersion " … "
               adaptCabalFile packageVersion
               isCompatible <- checkCabalFile
               putStrLn $ if isCompatible then yesSymbol else noSymbol
               return isCompatible
        yesSymbol = "✓"
        noSymbol = "✗"

untilWhile :: (Functor m, Monad m) => (a -> m Bool) -> [a] -> m ()
untilWhile predicate list
  = do rest <- Loops.dropWhileM notPredicate list
       case rest of
           [] -> return ()
           (_ : rest') -> do _ <- Loops.dropWhileM predicate rest'
                             return ()
  where notPredicate = fmap not . predicate

checkCabalFile :: IO Bool
checkCabalFile
  = Loops.andM
      [call "make" ["clean"], call "make" ["sandbox"], call "make" ["test"]]
