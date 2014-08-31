module Language.Haskell.Formatter.Process.AttachComments
       (attachComments) where
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Traversable as Traversable
import qualified Language.Haskell.Formatter.CommentCore
       as CommentCore
import qualified Language.Haskell.Formatter.ExactCode as ExactCode
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Process.Note as Note
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source

data Assignment = Assignment (Map.Map Location.SrcSpan
                                Note.CommentNote)
                deriving (Eq, Ord, Show)

instance Monoid.Monoid Assignment where
        mempty = Assignment Map.empty
        mappend (Assignment left) (Assignment right) = Assignment merged
          where merged = Map.unionWith Monoid.mappend left right

attachComments ::
               ExactCode.ExactCode -> Result.Result Code.CommentableCode
attachComments exact
  = if Map.null unassigned then return commentable else
      Result.fatalAssertionError message
  where (Assignment unassigned, commentable)
          = spread assignment locatable
        assignment = assignForCode exact
        locatable = ExactCode.actualCode exact
        message = "Attaching the comments failed with an unassigned rest."

spread ::
       Assignment ->
         Code.LocatableCode -> (Assignment, Code.CommentableCode)
spread (Assignment assignment) locatable
  = (Assignment unassigned, commentable)
  where (unassigned, commentable)
          = Traversable.mapAccumL move assignment locatable
        move rest nestedPortion = (rest', note)
          where (maybeNote, rest')
                  = Map.updateLookupWithKey remove portion rest
                remove = const . const Nothing
                portion = Location.getPortion nestedPortion
                note = Foldable.fold maybeNote

assignForCode :: ExactCode.ExactCode -> Assignment
assignForCode exact
  = Monoid.mappend (Monoid.mconcat untilLast) assignedAfterLast
  where ((maybeLast, afterLast), untilLast)
          = Traversable.mapAccumL move base orderedPortions
        move (maybeLower, rest) upper = ((Just upper, rest'), assignment)
          where (comments, rest')
                  = span ((<= upper) . Location.getPortion) rest
                assignment = assignComments maybeLower upper boxes
                boxes = createComments comments
        base = (Nothing, orderedComments)
        (orderedPortions, orderedComments) = orderByStartEnd exact
        assignedAfterLast = Foldable.foldMap assignLast maybeLast
        assignLast = flip assignAfter lastBoxes
        lastBoxes = createComments afterLast

assignComments ::
               Maybe Location.SrcSpan ->
                 Location.SrcSpan -> [Note.CommentBox] -> Assignment
assignComments Nothing upper comments = assignBefore upper comments
assignComments (Just lower) upper comments
  = Monoid.mappend assignedAfter assignedBefore
  where assignedAfter = assignAfter lower after
        (after, _, before) = divideComments comments
        assignedBefore = assignBefore upper before

assignBefore :: Location.SrcSpan -> [Note.CommentBox] -> Assignment
assignBefore portion = flip (assignSingleton portion) []

assignSingleton ::
                Location.SrcSpan ->
                  [Note.CommentBox] -> [Note.CommentBox] -> Assignment
assignSingleton portion before after
  = Assignment $ Map.singleton portion note
  where note = Note.createCommentNote before after

assignAfter :: Location.SrcSpan -> [Note.CommentBox] -> Assignment
assignAfter portion = assignSingleton portion []

divideComments ::
               [Note.CommentBox] ->
                 ([Note.CommentBox], [Note.CommentBox], [Note.CommentBox])
divideComments = divide [] []
  where divide after spaces [] = (after, spaces, [])
        divide after spaces
          rest@(box@(Note.ActualComment comment) : unwrapped)
          = case (after, spaces) of
                (_ : _, []) -> ifAfter
                _ -> case displacement of
                         CommentCore.BeforeActualCode -> ifBefore
                         CommentCore.AfterActualCode -> ifAfter
                         CommentCore.None -> ifBefore
          where ifAfter = divide (concat [after, spaces, [box]]) [] unwrapped
                displacement = CommentCore.documentationDisplacement core
                core = Note.commentCore comment
                ifBefore = (after, spaces, rest)
        divide after spaces (Note.EmptyLine : unwrapped)
          = divide after (Monoid.mappend spaces [Note.EmptyLine]) unwrapped

createComments :: [Source.Comment] -> [Note.CommentBox]
createComments
  = concat . snd . Traversable.mapAccumL create Nothing
  where create maybeEndLine comment = (Just endLine', comments)
          where endLine' = Location.getEndLine portion
                portion = Location.getPortion comment
                comments = Monoid.mappend emptyLines actualComments
                emptyLines
                  = case maybeEndLine of
                        Nothing -> []
                        Just endLine -> replicate emptyLineCount Note.EmptyLine
                          where emptyLineCount = pred lineDistance
                                lineDistance = Location.minus startLine endLine :: Int
                startLine = Location.getStartLine portion
                actualComments = [Note.ActualComment indentedComment]
                indentedComment = Note.createIndentedComment core Location.base
                core = Source.commentCore comment

orderByStartEnd ::
                ExactCode.ExactCode -> ([Location.SrcSpan], [Source.Comment])
orderByStartEnd exact = (orderedPortions, orderedComments)
  where orderedPortions = fmap Location.getPortion nestedPortions
        nestedPortions = List.sort $ Foldable.toList actualCode
        actualCode = ExactCode.actualCode exact
        orderedComments
          = List.sortBy (Function.on compare Location.getPortion) comments
        comments = ExactCode.comments exact
