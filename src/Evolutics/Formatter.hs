module Evolutics.Formatter (formatSource) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.SourceTree as SourceTree
import qualified Evolutics.Tools as Tools

formatSource :: Maybe FilePath -> String -> Either String String
formatSource maybeFile
  = format . Exts.parseFileContentsWithComments parseMode
  where format (Exts.ParseFailed location message)
          = Left $ Tools.showSourceLocation location message
        format (Exts.ParseOk (element, comments))
          = Right . show . formatTree $
              SourceTree.createSourceTree element comments
        parseMode
          = case maybeFile of
                Nothing -> Exts.defaultParseMode
                Just file -> Exts.defaultParseMode{Exts.parseFilename = file}

formatTree :: SourceTree.SourceTree -> SourceTree.SourceTree
formatTree sourceTree
  = SourceTree.createSourceTree element comments
  where element
          = Exts.fromParseResult . Exts.parseFileContents . Exts.prettyPrint
              $ SourceTree.element sourceTree
        comments = []
