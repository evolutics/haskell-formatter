module Evolutics.Code.Core
       ((Exts.=~=), Exts.amap, Exts.ann, Exts.Comment(..),
        Exts.defaultParseMode, Exts.exactPrint, Exts.fileName,
        Exts.fromParseResult, Exts.mkSrcSpan, Exts.Module,
        Exts.parseFileContents, Exts.parseFileContentsWithComments,
        Exts.parseFilename, Exts.ParseResult(..), Exts.prettyPrint,
        Exts.srcInfoPoints, Exts.srcInfoSpan, Exts.SrcLoc(..),
        Exts.SrcSpan, Exts.srcSpanEnd, Exts.srcSpanEndLine,
        Exts.SrcSpanInfo, Exts.srcSpanStart, Exts.srcSpanStartLine,
        Exts.startColumn, Exts.startLine)
       where
import qualified Language.Haskell.Exts.Annotated as Exts
