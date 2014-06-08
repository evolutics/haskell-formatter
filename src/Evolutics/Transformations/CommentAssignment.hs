module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.ConcreteCommented
       as ConcreteCommented

assignComments ::
               ConcreteCommented.ConcreteCommented -> Abstract.Abstract
assignComments
  = Abstract.create . fmap (const []) . ConcreteCommented.root
