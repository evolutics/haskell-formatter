module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Data.Traversable as Traversable
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Tools.SourceLocations as SourceLocations

assignComments :: Concrete.Commented -> Abstract.Code
assignComments concrete = Abstract.createCode newRoot
  where newRoot = Core.amap integrateRest intermediateRoot
        integrateRest = (++ map (abstractComment Abstract.After) rest)
        (rest, intermediateRoot)
          = Traversable.mapAccumL processElement comments oldRoot
        comments = Concrete.comments concrete
        oldRoot = Concrete.commentedRoot concrete

abstractComment ::
                Abstract.Displacement -> Core.Comment -> Abstract.Comment
abstractComment displacement concreteComment
  = Abstract.createComment displacement isMultiLine content
  where isMultiLine = Concrete.isCommentMultiLine concreteComment
        content = Concrete.commentContent concreteComment

processElement ::
               [Core.Comment] ->
                 Core.SrcSpanInfo -> ([Core.Comment], [Abstract.Comment])
processElement concreteComments nestedPortion
  = (remainder, abstractComments)
  where (choice, remainder)
          = span (follows nestedPortion) concreteComments
        abstractComments = map (abstractComment Abstract.Before) choice

follows :: Core.SrcSpanInfo -> Core.Comment -> Bool
follows nestedPortion comment
  = SourceLocations.comparePortions nestedPortion comment == GT
