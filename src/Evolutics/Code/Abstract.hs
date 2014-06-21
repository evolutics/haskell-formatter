module Evolutics.Code.Abstract
       (Code, codeRoot, Comment, commentDisplacement, commentCore,
        Displacement(..), createCode, createComment)
       where
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core

data Code = Code{codeRoot :: Core.Module [Comment]}

data Comment = Comment{commentDisplacement :: Displacement,
                       commentCore :: Comment.Comment}

data Displacement = Before
                  | After

createCode :: Core.Module [Comment] -> Code
createCode root = Code{codeRoot = root}

createComment :: Displacement -> Comment.Comment -> Comment
createComment displacement core
  = Comment{commentDisplacement = displacement, commentCore = core}
