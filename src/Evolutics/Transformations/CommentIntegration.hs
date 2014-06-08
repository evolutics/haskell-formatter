module Evolutics.Transformations.CommentIntegration
       (integrateComments) where
import qualified Evolutics.Code.Abstract.Abstract as Abstract
import qualified Evolutics.Code.Concrete.Commented as Commented
import qualified Evolutics.Code.Concrete.Commentless as Commentless

integrateComments ::
                  Abstract.Abstract -> Commentless.Commentless -> Commented.Commented
integrateComments _ concreteCommentless
  = Commented.create (Commentless.root concreteCommentless) []
