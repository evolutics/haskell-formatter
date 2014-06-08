module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Evolutics.Code.Abstract.Abstract as Abstract
import qualified Evolutics.Code.Concrete.Commented as Commented

assignComments :: Commented.Commented -> Abstract.Abstract
assignComments = Abstract.create . fmap (const []) . Commented.root
