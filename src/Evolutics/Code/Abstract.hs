module Evolutics.Code.Abstract
       (Code, codeRoot, Comment, commentDisplacement, Displacement(..),
        createCode, createComment, commentLineCount)
       where
import qualified Language.Haskell.Exts.Annotated as Exts
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Tools.Newlines as Newlines

data Code = Code{codeRoot :: Exts.Module [Comment]}

data Comment = Comment{commentDisplacement :: Displacement,
                       isCommentMultiLine :: Bool, commentContent :: String}

data Displacement = Before
                  | After

createCode :: Exts.Module [Comment] -> Code
createCode root = Code{codeRoot = root}

createComment :: Displacement -> Bool -> String -> Comment
createComment displacement isMultiLine content
  = Comment{commentDisplacement = displacement,
            isCommentMultiLine = isMultiLine, commentContent = content}

commentLineCount :: Comment -> Int
commentLineCount
  = length . Newlines.splitSeparatedLines . commentContent
