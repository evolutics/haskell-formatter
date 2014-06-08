module Evolutics.Transformations.CommentIntegration
       (integrateComments) where
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.ConcreteCommented
       as ConcreteCommented
import qualified Evolutics.Code.ConcreteCommentless
       as ConcreteCommentless

integrateComments ::
                  Abstract.Abstract ->
                    ConcreteCommentless.ConcreteCommentless ->
                      ConcreteCommented.ConcreteCommented
integrateComments _ concreteCommentless
  = ConcreteCommented.create
      (ConcreteCommentless.root concreteCommentless)
      []
