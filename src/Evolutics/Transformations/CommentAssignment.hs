module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete

assignComments :: Concrete.Commented -> Abstract.Code
assignComments
  = Abstract.create . fmap (const []) . Concrete.commentedRoot
