module Evolutics.Code.Abstract
       (Code, codeRoot, Comment, commentDisplacement, Displacement(..),
        createCode, createComment, commentKind, commentContent,
        commentLineCount)
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

createComment :: Displacement -> Comment.Kind -> String -> Comment
createComment displacement kind content
  = Comment{commentDisplacement = displacement, commentCore = core}
  where core = Comment.create kind content

commentKind :: Comment -> Comment.Kind
commentKind = Comment.kind . commentCore

commentContent :: Comment -> String
commentContent = Comment.content . commentCore

commentLineCount :: Comment -> Int
commentLineCount = Comment.lineCount . commentCore
