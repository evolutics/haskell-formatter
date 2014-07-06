module Evolutics.Code.Concrete
       (Commented, commentedRoot, comments, Commentless, commentlessRoot,
        createCommented, createCommentless, makeCommentless)
       where
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source

data Commented = Commented{commentedRoot ::
                           Source.Module Location.SrcSpanInfo,
                           comments :: [Source.Comment]}
               deriving Eq

data Commentless = Commentless{commentlessRoot ::
                               Source.Module Location.SrcSpanInfo}
                 deriving (Eq, Ord, Show)

instance Show Commented where
        show commented
          = Source.exactPrint (commentedRoot commented) $ comments commented

instance Location.Portioned Commentless where
        getPortion = Location.getPortion . commentlessRoot

createCommented ::
                Source.Module Location.SrcSpanInfo -> [Source.Comment] -> Commented
createCommented root commentList
  = Commented{commentedRoot = root, comments = commentList}

createCommentless ::
                  Source.Module Location.SrcSpanInfo -> Commentless
createCommentless root = Commentless{commentlessRoot = root}

makeCommentless :: Commented -> Commentless
makeCommentless = createCommentless . commentedRoot
