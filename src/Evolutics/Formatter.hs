module Evolutics.Formatter (formatSource) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.ConcreteCommented
       as ConcreteCommented
import qualified Evolutics.Code.ConcreteCommentless
       as ConcreteCommentless
import qualified Evolutics.Tools as Tools

formatSource :: Maybe FilePath -> String -> Either String String
formatSource maybeFile
  = format . Exts.parseFileContentsWithComments parseMode
  where format (Exts.ParseFailed location message)
          = Left $ Tools.formatSourceMessage location message
        format (Exts.ParseOk (root, comments))
          = Right . rawFormat $
              ConcreteCommented.ConcreteCommented root comments
        parseMode
          = case maybeFile of
                Nothing -> Exts.defaultParseMode
                Just file -> Exts.defaultParseMode{Exts.parseFilename = file}

rawFormat :: ConcreteCommented.ConcreteCommented -> String
rawFormat code = Exts.exactPrint root comments
  where ConcreteCommented.ConcreteCommented root comments
          = formatCode code

formatCode ::
           ConcreteCommented.ConcreteCommented ->
             ConcreteCommented.ConcreteCommented
formatCode concreteCommented
  = integrateComments abstract concreteCommentless
  where abstract = assignComments concreteCommented
        concreteCommentless
          = arrangeElements $ dropComments concreteCommented

integrateComments ::
                  Abstract.Abstract ->
                    ConcreteCommentless.ConcreteCommentless ->
                      ConcreteCommented.ConcreteCommented
integrateComments _ (ConcreteCommentless.ConcreteCommentless root)
  = ConcreteCommented.ConcreteCommented root []

assignComments ::
               ConcreteCommented.ConcreteCommented -> Abstract.Abstract
assignComments (ConcreteCommented.ConcreteCommented root _)
  = Abstract.create $ fmap (const []) root

arrangeElements ::
                ConcreteCommentless.ConcreteCommentless ->
                  ConcreteCommentless.ConcreteCommentless
arrangeElements (ConcreteCommentless.ConcreteCommentless root)
  = ConcreteCommentless.ConcreteCommentless .
      Exts.fromParseResult . Exts.parseFileContents
      $ Exts.prettyPrint root

dropComments ::
             ConcreteCommented.ConcreteCommented ->
               ConcreteCommentless.ConcreteCommentless
dropComments (ConcreteCommented.ConcreteCommented root comments)
  = ConcreteCommentless.ConcreteCommentless root
