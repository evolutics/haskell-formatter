module Evolutics.Formatter (formatSource) where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Tools as Tools

data ConcreteCommented = ConcreteCommented (Exts.Module
                                              Exts.SrcSpanInfo)
                                           [Exts.Comment]

data ConcreteCommentless = ConcreteCommentless (Exts.Module
                                                  Exts.SrcSpanInfo)

data Abstract = Abstract (Exts.Module [AbstractComment])

data AbstractComment = AbstractComment Displacement Bool String

data Displacement = Before
                  | After

formatSource :: Maybe FilePath -> String -> Either String String
formatSource maybeFile
  = format . Exts.parseFileContentsWithComments parseMode
  where format (Exts.ParseFailed location message)
          = Left $ Tools.formatSourceMessage location message
        format (Exts.ParseOk (root, comments))
          = Right . rawFormat $ ConcreteCommented root comments
        parseMode
          = case maybeFile of
                Nothing -> Exts.defaultParseMode
                Just file -> Exts.defaultParseMode{Exts.parseFilename = file}

rawFormat :: ConcreteCommented -> String
rawFormat code = Exts.exactPrint root comments
  where ConcreteCommented root comments = formatCode code

formatCode :: ConcreteCommented -> ConcreteCommented
formatCode concreteCommented
  = integrateComments abstract concreteCommentless
  where abstract = assignComments concreteCommented
        concreteCommentless
          = arrangeElements $ dropComments concreteCommented

integrateComments ::
                  Abstract -> ConcreteCommentless -> ConcreteCommented
integrateComments _ (ConcreteCommentless root)
  = ConcreteCommented root []

assignComments :: ConcreteCommented -> Abstract
assignComments (ConcreteCommented root _)
  = Abstract $ fmap (const []) root

arrangeElements :: ConcreteCommentless -> ConcreteCommentless
arrangeElements (ConcreteCommentless root)
  = ConcreteCommentless .
      Exts.fromParseResult . Exts.parseFileContents
      $ Exts.prettyPrint root

dropComments :: ConcreteCommented -> ConcreteCommentless
dropComments (ConcreteCommented root comments)
  = ConcreteCommentless root
