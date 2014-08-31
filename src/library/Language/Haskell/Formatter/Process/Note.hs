{-|
Description : Annotations of syntax trees
-}
module Language.Haskell.Formatter.Process.Note
       (CommentNote, commentsBefore, commentsAfter, CommentBox(..),
        IndentedComment, commentCore, commentStartColumn, LocationCommentNote,
        locationNote, commentNote, createCommentNote, createIndentedComment,
        createLocationCommentNote, replaceCommentBoxes,
        replaceCommentStartColumn, replaceCommentNote)
       where
import qualified Data.Function as Function
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.CommentCore as CommentCore
import qualified Language.Haskell.Formatter.Location as Location

data CommentNote = CommentNote{commentsBefore :: [CommentBox],
                               commentsAfter :: [CommentBox]}
                 deriving (Eq, Ord, Show)

data CommentBox = ActualComment IndentedComment
                | EmptyLine
                deriving (Eq, Ord, Show)

data IndentedComment = IndentedComment{commentCore :: CommentCore.CommentCore,
                                       commentStartColumn :: Location.Column}
                     deriving (Eq, Ord, Show)

data LocationCommentNote = LocationCommentNote{locationNote ::
                                               Location.SrcSpanInfo,
                                               commentNote :: CommentNote}
                         deriving (Eq, Ord, Show)

instance Monoid.Monoid CommentNote where
        mempty = createCommentNote [] []
        mappend left right = createCommentNote before after
          where before = merge commentsBefore
                merge getComments = Function.on (++) getComments left right
                after = merge commentsAfter

instance Location.Portioned LocationCommentNote where
        getPortion = Location.getPortion . locationNote

createCommentNote :: [CommentBox] -> [CommentBox] -> CommentNote
createCommentNote rawCommentsBefore rawCommentsAfter
  = CommentNote{commentsBefore = rawCommentsBefore,
                commentsAfter = rawCommentsAfter}

createIndentedComment ::
                      CommentCore.CommentCore ->
                        Location.Column -> IndentedComment
createIndentedComment rawCommentCore rawCommentStartColumn
  = IndentedComment{commentCore = rawCommentCore,
                    commentStartColumn = rawCommentStartColumn}

createLocationCommentNote ::
                          Location.SrcSpanInfo ->
                            CommentNote -> LocationCommentNote
createLocationCommentNote rawLocationNote rawCommentNote
  = LocationCommentNote{locationNote = rawLocationNote,
                        commentNote = rawCommentNote}

replaceCommentBoxes ::
                    ([CommentBox] -> [CommentBox]) -> CommentNote -> CommentNote
replaceCommentBoxes function note
  = note{commentsBefore = replace commentsBefore,
         commentsAfter = replace commentsAfter}
  where replace getComments = function $ getComments note

replaceCommentBox :: (CommentBox -> CommentBox) -> CommentNote -> CommentNote
replaceCommentBox function = replaceCommentBoxes $ fmap function

replaceIndentedComment ::
                       (IndentedComment -> IndentedComment) ->
                         CommentNote -> CommentNote
replaceIndentedComment function = replaceCommentBox partFunction
  where partFunction (ActualComment comment) = ActualComment $ function comment
        partFunction EmptyLine = EmptyLine

replaceCommentStartColumn ::
                          (Location.Column -> Location.Column) ->
                            CommentNote -> CommentNote
replaceCommentStartColumn function = replaceIndentedComment partFunction
  where partFunction comment
          = comment{commentStartColumn = function $ commentStartColumn comment}

replaceCommentNote ::
                   (CommentNote -> CommentNote) ->
                     LocationCommentNote -> LocationCommentNote
replaceCommentNote function note
  = note{commentNote = function $ commentNote note}
