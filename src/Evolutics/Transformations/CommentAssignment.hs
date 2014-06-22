module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Data.Traversable as Traversable
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Code.Locations as Locations

assignComments :: Concrete.Commented -> Abstract.Code
assignComments concrete = Abstract.createCode newRoot
  where newRoot = Core.amap integrateRest intermediateRoot
        integrateRest annotation
          = Abstract.createAnnotation (Abstract.commentsBefore annotation) $
              Abstract.commentsAfter annotation ++ abstractComments rest
        (rest, intermediateRoot)
          = Traversable.mapAccumL createAnnotation comments oldRoot
        comments = Concrete.comments concrete
        oldRoot = Concrete.commentedRoot concrete

abstractComments :: [Core.Comment] -> [Abstract.Comment]
abstractComments = map abstractComment
  where abstractComment
          = Abstract.createComment Locations.firstColumn .
              Concrete.commentCore

createAnnotation ::
                 [Core.Comment] ->
                   Core.SrcSpanInfo -> ([Core.Comment], Abstract.Annotation)
createAnnotation comments nestedPortion = (rest, annotation)
  where (choice, rest) = span (follows nestedPortion) comments
        annotation = Abstract.createAnnotation before after
        before = abstractComments choice
        after = []

follows :: Core.SrcSpanInfo -> Core.Comment -> Bool
follows nestedPortion comment
  = Locations.comparePortions nestedPortion comment == GT
