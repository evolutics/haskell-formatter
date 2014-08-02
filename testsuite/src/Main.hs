module Main (main) where
import qualified Language.Haskell.Formatter.Tests as Formatter
import qualified Test.Tasty as Tasty

main :: IO ()
main
  = do roots <- sequence tests
       Tasty.defaultMain $ Tasty.testGroup "Root" roots

tests :: [IO Tasty.TestTree]
tests = [Formatter.tests]
