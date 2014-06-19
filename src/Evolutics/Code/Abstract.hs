module Evolutics.Code.Abstract
       (Code, codeRoot, Comment, commentDisplacement, isCommentMultiLine,
        commentContent, Displacement(..), createCode, createComment,
        commentLineCount)
       where
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Tools.Newlines as Newlines

data Code = Code{codeRoot :: Core.Module [Comment]}

data Comment = Comment{commentDisplacement :: Displacement,
                       isCommentMultiLine :: Bool, commentContent :: String}

data Displacement = Before
                  | After

createCode :: Core.Module [Comment] -> Code
createCode root = Code{codeRoot = root}

createComment :: Displacement -> Bool -> String -> Comment
createComment displacement isMultiLine content
  = Comment{commentDisplacement = displacement,
            isCommentMultiLine = isMultiLine, commentContent = content}

commentLineCount :: Comment -> Int
commentLineCount
  = length . Newlines.splitSeparatedLines . commentContent
