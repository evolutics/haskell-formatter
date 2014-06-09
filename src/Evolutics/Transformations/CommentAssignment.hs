module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Data.Traversable as Traversable
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Tools.SourceLocations as SourceLocations

assignComments :: Concrete.Commented -> Abstract.Code
assignComments concrete = Abstract.createCode newRoot
  where newRoot = Exts.amap integrateRest intermediateRoot
        integrateRest = (++ map (abstractComment Abstract.After) rest)
        (rest, intermediateRoot)
          = Traversable.mapAccumL processElement comments oldRoot
        comments = Concrete.comments concrete
        oldRoot = Concrete.commentedRoot concrete

abstractComment ::
                Abstract.Displacement -> Exts.Comment -> Abstract.Comment
abstractComment displacement concreteComment
  = Abstract.createComment displacement isMultiLine content
  where isMultiLine = Concrete.isCommentMultiLine concreteComment
        content = Concrete.commentContent concreteComment

processElement ::
               [Exts.Comment] ->
                 Exts.SrcSpanInfo -> ([Exts.Comment], [Abstract.Comment])
processElement concreteComments elementPortion
  = (remainder, abstractComments)
  where (choice, remainder)
          = span (follows elementPortion) concreteComments
        abstractComments = map (abstractComment Abstract.Before) choice

follows :: Exts.SrcSpanInfo -> Exts.Comment -> Bool
follows portion comment
  = SourceLocations.comparePortions portion comment == GT
