module Language.Haskell.Formatter.Code.Merged
       (Code, codeRoot, Part, partAnnotation, partNestedPortion,
        createCode, mergeCode, makeCommentless, createPart)
       where
import qualified Language.Haskell.Formatter.Code.Abstract
       as Abstract
import qualified Language.Haskell.Formatter.Code.Concrete
       as Concrete
import qualified Language.Haskell.Formatter.Code.Location
       as Location
import qualified Language.Haskell.Formatter.Code.Source as Source
import qualified Language.Haskell.Formatter.Toolkit.FunctionTool
       as FunctionTool

data Code = Code{codeRoot :: Source.Module Part}
          deriving (Eq, Ord, Show)

data Part = Part{partAnnotation :: Abstract.Annotation,
                 partNestedPortion :: Location.SrcSpanInfo}
          deriving (Eq, Ord, Show)

instance Location.Portioned Part where
        getPortion = Location.getPortion . partNestedPortion

createCode :: Source.Module Part -> Code
createCode root = Code{codeRoot = root}

mergeCode :: Abstract.Code -> Concrete.Commentless -> Maybe Code
mergeCode abstract commentless
  | abstractRoot Source.=~= commentlessRoot = fmap createCode root
  where root
          = FunctionTool.halfZipWith createPart abstractRoot commentlessRoot
        abstractRoot = Abstract.codeRoot abstract
        commentlessRoot = Concrete.commentlessRoot commentless
mergeCode _ _ = Nothing

makeCommentless :: Code -> Concrete.Commentless
makeCommentless
  = Concrete.createCommentless . fmap partNestedPortion . codeRoot

createPart :: Abstract.Annotation -> Location.SrcSpanInfo -> Part
createPart annotation nestedPortion
  = Part{partAnnotation = annotation,
         partNestedPortion = nestedPortion}
