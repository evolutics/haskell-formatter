module Evolutics.Code.Merged
       (Code, codeRoot, Part, partAnnotation, createCode, mergeCode,
        makeCommentless, createPart)
       where
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source
import qualified Evolutics.Tools.Functions as Functions

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
          = Functions.halfZipWith createPart abstractRoot commentlessRoot
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
