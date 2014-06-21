module Evolutics.Code.Abstract
       (Code, codeRoot, Comment, commentDisplacement, commentKind,
        commentContent, Displacement(..), createCode, createComment,
        commentLineCount)
       where
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Tools.Newlines as Newlines

data Code = Code{codeRoot :: Core.Module [Comment]}

data Comment = Comment{commentDisplacement :: Displacement,
                       commentKind :: Comment.Kind, commentContent :: String}

data Displacement = Before
                  | After

createCode :: Core.Module [Comment] -> Code
createCode root = Code{codeRoot = root}

createComment :: Displacement -> Comment.Kind -> String -> Comment
createComment displacement kind content
  = Comment{commentDisplacement = displacement, commentKind = kind,
            commentContent = content}

commentLineCount :: Comment -> Int
commentLineCount
  = length . Newlines.splitSeparatedLines . commentContent
