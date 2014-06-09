module Evolutics.Transformations.CommentIntegration
       (integrateComments) where
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Concrete as Concrete

integrateComments ::
                  Abstract.Code -> Concrete.Commentless -> Concrete.Commented
integrateComments _ concreteCommentless
  = Concrete.createCommented
      (Concrete.commentlessRoot concreteCommentless)
      []
