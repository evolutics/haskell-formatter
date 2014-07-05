module Evolutics.Code.Concrete
       (Commented, commentedRoot, comments, Commentless, commentlessRoot,
        createCommented, createCommentless, makeCommentless)
       where
import qualified Evolutics.Code.Source as Source

data Commented = Commented{commentedRoot ::
                           Source.Module Source.SrcSpanInfo,
                           comments :: [Source.Comment]}

data Commentless = Commentless{commentlessRoot ::
                               Source.Module Source.SrcSpanInfo}

instance Show Commented where
        show commented
          = Source.exactPrint (commentedRoot commented) $ comments commented

instance Source.Portioned Commentless where
        getPortion = Source.getPortion . commentlessRoot

createCommented ::
                Source.Module Source.SrcSpanInfo -> [Source.Comment] -> Commented
createCommented root commentList
  = Commented{commentedRoot = root, comments = commentList}

createCommentless ::
                  Source.Module Source.SrcSpanInfo -> Commentless
createCommentless root = Commentless{commentlessRoot = root}

makeCommentless :: Commented -> Commentless
makeCommentless = createCommentless . commentedRoot
