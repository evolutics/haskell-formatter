module Main (main) where
import qualified Language.Haskell.Formatter.Tests as Formatter
import qualified Test.Tasty as Tasty

main :: IO ()
main
  = sequence tests >>= Tasty.defaultMain . Tasty.testGroup "Root"

tests :: [IO Tasty.TestTree]
tests = [Formatter.tests]
