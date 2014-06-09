module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Data.Traversable as Traversable
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Tools.SourceLocations as SourceLocations

assignComments :: Concrete.Commented -> Abstract.Code
assignComments concrete = Abstract.createCode root'
  where root = Concrete.commentedRoot concrete
        comments = Concrete.comments concrete
        (remainder, root')
          = Traversable.mapAccumL processElement comments root

processElement ::
               [Exts.Comment] ->
                 Exts.SrcSpanInfo -> ([Exts.Comment], [Abstract.Comment])
processElement concreteComments elementPortion
  = (remainder, abstractComments)
  where (choice, remainder)
          = span (follows elementPortion) concreteComments
        abstractComments = []

follows :: Exts.SrcSpanInfo -> Exts.Comment -> Bool
follows portion comment
  = SourceLocations.comparePortions portion comment == GT
