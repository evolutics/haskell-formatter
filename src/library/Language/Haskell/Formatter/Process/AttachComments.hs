{-|
Description : Attaching comments to the annotations of the syntax tree

This is the first part of the process.
-}
module Language.Haskell.Formatter.Process.AttachComments (attachComments) where
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Semigroup as Semigroup
import qualified Data.Traversable as Traversable
import qualified Language.Haskell.Formatter.CommentCore as CommentCore
import qualified Language.Haskell.Formatter.ExactCode as ExactCode
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Process.LineTool as LineTool
import qualified Language.Haskell.Formatter.Process.Note as Note
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Style as Style

newtype Assignment = Assignment (Map.Map Location.SrcSpan Note.CommentNote)
                       deriving (Eq, Ord, Show)

data CodeGap = InfiniteLower Location.SrcSpan
             | FiniteGap Location.SrcSpan Location.SrcSpan
             | InfiniteUpper Location.SrcSpan
                 deriving (Eq, Ord, Show)

instance Semigroup.Semigroup Assignment where
        (Assignment left) <> (Assignment right) = Assignment merged
          where merged = Map.unionWith Monoid.mappend left right

instance Monoid.Monoid Assignment where
        mempty = Assignment Map.empty

attachComments ::
               Style.Style ->
                 ExactCode.ExactCode -> Result.Result Code.CommentableCode
attachComments _ exact
  = do assignment <- assignForCode exact
       let (Assignment unassigned, commentable) = spread assignment locatable
       if Map.null unassigned then return commentable else
         Result.fatalAssertionError message
  where locatable = ExactCode.actualCode exact
        message = "Attaching the comments failed with an unassigned rest."

assignForCode :: ExactCode.ExactCode -> Result.Result Assignment
assignForCode exact
  = case unassigned of
        [] -> return $ Monoid.mconcat assignments
        (_ : _) -> Result.fatalAssertionError message
  where ((_, unassigned), assignments)
          = Traversable.mapAccumL move base maybeOrderedPortions
        move (maybeLower, rest) maybeUpper = ((maybeUpper, rest'), assignment)
          where (rest', assignment)
                  = case maybeUpper of
                        Nothing -> case maybeLower of
                                       Nothing -> (rest, Monoid.mempty)
                                       Just lower -> ([],
                                                      assign
                                                        (InfiniteUpper lower)
                                                        rest)
                        Just upper -> (greaterUpper, assign gap lessEqualUpper)
                          where (lessEqualUpper, greaterUpper)
                                  = span ((<= upper) . Location.getPortion) rest
                                gap
                                  = case maybeLower of
                                        Nothing -> InfiniteLower upper
                                        Just lower -> FiniteGap lower upper
        assign gap = assignComments gap . createComments startLine endLine
          where (startLine, endLine)
                  = case gap of
                        InfiniteLower upper -> (pred Location.base,
                                                Location.getStartLine upper)
                        FiniteGap lower upper -> (Location.getEndLine lower,
                                                  Location.getStartLine upper)
                        InfiniteUpper lower -> (Location.getEndLine lower,
                                                codeEndLine)
        codeEndLine = Location.getEndLine $ Location.getPortion exact
        base = (Nothing, orderedComments)
        (orderedPortions, orderedComments) = orderByStartEnd exact
        maybeOrderedPortions
          = Monoid.mappend (fmap Just orderedPortions) [Nothing]
        message = "Assigning the comments failed with an unexpected rest."

assignComments :: CodeGap -> [Note.CommentBox] -> Assignment
assignComments (InfiniteLower upper) comments = assignBefore upper comments
assignComments (FiniteGap lower upper) comments
  = Monoid.mappend assignedAfter assignedBefore
  where assignedAfter = assignAfter lower after
        (after, spaces, before) = divideComments comments
        assignedBefore = assignBefore upper $ Monoid.mappend spaces before
assignComments (InfiniteUpper lower) comments = assignAfter lower comments

assignBefore :: Location.SrcSpan -> [Note.CommentBox] -> Assignment
assignBefore portion = flip (assignSingleton portion) []

assignSingleton ::
                Location.SrcSpan ->
                  [Note.CommentBox] -> [Note.CommentBox] -> Assignment
assignSingleton portion before after = Assignment $ Map.singleton portion note
  where note = Note.createCommentNote before after

assignAfter :: Location.SrcSpan -> [Note.CommentBox] -> Assignment
assignAfter portion = assignSingleton portion []

divideComments ::
               [Note.CommentBox] ->
                 ([Note.CommentBox], [Note.CommentBox], [Note.CommentBox])
divideComments = divide [] []
  where divide after spaces [] = (after, spaces, [])
        divide after spaces rest@(box@(Note.ActualComment comment) : unwrapped)
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

createComments ::
               Location.Line ->
                 Location.Line -> [Source.Comment] -> [Note.CommentBox]
createComments gapStartLine gapEndLine comments
  = Monoid.mappend (concat untilLast) lastBoxes
  where (lastEndLine, untilLast)
          = Traversable.mapAccumL create gapStartLine comments
        create endLine comment = (endLine', boxes)
          where endLine' = Location.getEndLine portion
                portion = Location.getPortion comment
                boxes = Monoid.mappend emptyLines actualComments
                emptyLines = createEmptyLines endLine startLine
                startLine = Location.getStartLine portion
                actualComments = [Note.ActualComment indentedComment]
                indentedComment = Note.createIndentedComment core Location.base
                core = Source.commentCore comment
        lastBoxes = createEmptyLines lastEndLine gapEndLine

createEmptyLines :: Location.Line -> Location.Line -> [Note.CommentBox]
createEmptyLines endLine startLine = replicate emptyLineCount Note.EmptyLine
  where emptyLineCount = LineTool.countEmptyLines endLine startLine

orderByStartEnd :: ExactCode.ExactCode -> ([Location.SrcSpan], [Source.Comment])
orderByStartEnd exact = (orderedPortions, orderedComments)
  where orderedPortions = fmap Location.getPortion nestedPortions
        nestedPortions = List.sort $ Foldable.toList actualCode
        actualCode = ExactCode.actualCode exact
        orderedComments
          = List.sortBy (Function.on compare Location.getPortion) comments
        comments = ExactCode.comments exact

spread :: Assignment -> Code.LocatableCode -> (Assignment, Code.CommentableCode)
spread (Assignment assignment) locatable = (Assignment unassigned, commentable)
  where (unassigned, commentable)
          = Traversable.mapAccumL move assignment locatable
        move rest nestedPortion = (rest', note)
          where (maybeNote, rest') = Map.updateLookupWithKey remove portion rest
                remove = const . const Nothing
                portion = Location.getPortion nestedPortion
                note = Foldable.fold maybeNote
