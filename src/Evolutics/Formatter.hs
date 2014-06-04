{-# LANGUAGE ScopedTypeVariables #-}
module Evolutics.Formatter (formatSource) where
import qualified Data.Data as Data
import qualified Data.Generics as Generics
import qualified Debug.Trace as Trace
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.SourceTree as SourceTree
import qualified Evolutics.Tools as Tools

formatSource :: Maybe FilePath -> String -> Either String String
formatSource maybeFile
  = format . Exts.parseFileContentsWithComments parseMode
  where format (Exts.ParseFailed location message)
          = Left $ Tools.formatSourceMessage location message
        format (Exts.ParseOk (element, comments))
          = Right . show . formatTree $ attachComments element' comments
          where element' = Trace.trace (show $ visit element) element
        parseMode
          = case maybeFile of
                Nothing -> Exts.defaultParseMode
                Just file -> Exts.defaultParseMode{Exts.parseFilename = file}

formatTree :: SourceTree.SourceTree -> SourceTree.SourceTree
formatTree sourceTree = fromElementOnly element
  where element
          = Exts.fromParseResult . Exts.parseFileContents . Exts.prettyPrint
              $ SourceTree.toElementOnly sourceTree

fromElementOnly ::
                Exts.Module Exts.SrcSpanInfo -> SourceTree.SourceTree
fromElementOnly element
  = SourceTree.SourceTree $ fmap (flip (,) []) element

attachComments ::
               Exts.Module Exts.SrcSpanInfo ->
                 [Exts.Comment] -> SourceTree.SourceTree
attachComments element _ = fromElementOnly element

visit :: (Data.Data a) => a -> [String]
visit = Generics.everything (++) query

query :: (Data.Data a) => a -> [String]
query = Generics.mkQ [] select
  where select
          (Exts.Ident _ identifier :: Exts.Name Exts.SrcSpanInfo)
          = [identifier]
        select _ = []
