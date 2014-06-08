module Evolutics.Formatting (formatSource) where
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
          = Right . show . formatCode $
              ConcreteCommented.create root comments
        parseMode
          = case maybeFile of
                Nothing -> Exts.defaultParseMode
                Just file -> Exts.defaultParseMode{Exts.parseFilename = file}

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
integrateComments _ concreteCommentless
  = ConcreteCommented.create
      (ConcreteCommentless.root concreteCommentless)
      []

assignComments ::
               ConcreteCommented.ConcreteCommented -> Abstract.Abstract
assignComments
  = Abstract.create . fmap (const []) . ConcreteCommented.root

arrangeElements ::
                ConcreteCommentless.ConcreteCommentless ->
                  ConcreteCommentless.ConcreteCommentless
arrangeElements
  = ConcreteCommentless.create .
      Exts.fromParseResult .
        Exts.parseFileContents .
          Exts.prettyPrint . ConcreteCommentless.root

dropComments ::
             ConcreteCommented.ConcreteCommented ->
               ConcreteCommentless.ConcreteCommentless
dropComments = ConcreteCommentless.create . ConcreteCommented.root
