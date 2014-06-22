module Evolutics.Code.Merged
       (Code, codeRoot, Part, annotation, createCode) where
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Code.Locations as Locations
import qualified Evolutics.Tools.Functions as Functions

data Code = Code{codeRoot :: Core.Module Part}

data Part = Part{annotation :: Abstract.Annotation,
                 nestedPortion :: Core.SrcSpanInfo}

instance Locations.Portioned Part where
        portion = Locations.portion . nestedPortion

createCode :: Abstract.Code -> Concrete.Commentless -> Maybe Code
createCode abstract commentless
  | abstractRoot Core.=~= commentlessRoot =
    Just Code{codeRoot = root}
  where root
          = Functions.halfZipWith createPart abstractRoot commentlessRoot
        abstractRoot = Abstract.codeRoot abstract
        commentlessRoot = Concrete.commentlessRoot commentless
createCode _ _ = Nothing

createPart :: Abstract.Annotation -> Core.SrcSpanInfo -> Part
createPart annotation nestedPortion
  = Part{annotation = annotation, nestedPortion = nestedPortion}
