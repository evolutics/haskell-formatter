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
        integrateRest = (++ map (abstractComment Abstract.After) rest)
        (rest, intermediateRoot)
          = Traversable.mapAccumL processElement comments oldRoot
        comments = Concrete.comments concrete
        oldRoot = Concrete.commentedRoot concrete

abstractComment ::
                Abstract.Displacement -> Core.Comment -> Abstract.Comment
abstractComment displacement
  = Abstract.createComment displacement . Concrete.commentCore

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
  = Locations.comparePortions nestedPortion comment == GT
