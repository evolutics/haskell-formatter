module Evolutics.Formatter (formatSource) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.SourceTree as SourceTree
import qualified Evolutics.Tools as Tools

formatSource :: Maybe String -> String -> Either String String
formatSource maybeSourceName
  = checkToFormat . Exts.parseFileContentsWithComments parseMode
  where checkToFormat (Exts.ParseFailed location message)
          = Left $ Tools.showSourceLocation location message
        checkToFormat (Exts.ParseOk (element, comments))
          = Right . show . format $
              SourceTree.createSourceTree element comments
        parseMode
          = case maybeSourceName of
                Nothing -> Exts.defaultParseMode
                Just sourceName -> Exts.defaultParseMode{Exts.parseFilename =
                                                           sourceName}

format :: SourceTree.SourceTree -> SourceTree.SourceTree
format sourceTree = SourceTree.createSourceTree element comments
  where element
          = Exts.fromParseResult . Exts.parseFileContents . Exts.prettyPrint
              $ SourceTree.element sourceTree
        comments = []
