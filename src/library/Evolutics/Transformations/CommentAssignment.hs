module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Data.Traversable as Traversable
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Locations as Locations
import qualified Evolutics.Code.Source as Source

assignComments :: Concrete.Commented -> Abstract.Code
assignComments concrete = Abstract.createCode root''
  where root'' = Source.amap integrateRest root'
        integrateRest annotation
          = annotation{Abstract.commentsAfter =
                         Abstract.commentsAfter annotation ++ abstractComments rest}
        (rest, root')
          = Traversable.mapAccumL createAnnotation comments root
        comments = Concrete.comments concrete
        root = Concrete.commentedRoot concrete

abstractComments :: [Source.Comment] -> [Abstract.Comment]
abstractComments = map abstractComment
  where abstractComment
          = Abstract.createComment Source.zero . Source.commentCore

createAnnotation ::
                 [Source.Comment] ->
                   Source.SrcSpanInfo -> ([Source.Comment], Abstract.Annotation)
createAnnotation comments nestedPortion = (rest, annotation)
  where (choice, rest) = span (follows nestedPortion) comments
        annotation = Abstract.createAnnotation before after
        before = abstractComments choice
        after = []

follows :: Source.SrcSpanInfo -> Source.Comment -> Bool
follows nestedPortion comment
  = Locations.comparePortions elementPortion commentPortion == GT
  where elementPortion = Source.getPortion nestedPortion
        commentPortion = Source.getPortion comment
