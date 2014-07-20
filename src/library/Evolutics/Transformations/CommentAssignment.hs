module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe as Maybe
import qualified Data.Monoid as Monoid
import qualified Data.Traversable as Traversable
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source

data Assignment = Assignment (Map.Map Location.SrcSpan
                                Abstract.Annotation)
                deriving (Eq, Ord, Show)

assignComments :: Concrete.Commented -> Abstract.Code
assignComments commented = snd $ spread assignment commentless
  where assignment = createAssignment commented
        commentless = Concrete.makeCommentless commented

spread ::
       Assignment -> Concrete.Commentless -> (Assignment, Abstract.Code)
spread (Assignment assignment) commentless
  = (Assignment unassigned, Abstract.createCode abstractRoot)
  where (unassigned, abstractRoot)
          = Traversable.mapAccumL move assignment concreteRoot
        move rest nestedPortion = (rest', annotation)
          where (maybeAnnotation, rest')
                  = Map.updateLookupWithKey remove portion rest
                remove = const . const Nothing
                portion = Location.getPortion nestedPortion
                annotation = Foldable.fold maybeAnnotation
        concreteRoot = Concrete.commentlessRoot commentless

createAssignment :: Concrete.Commented -> Assignment
createAssignment commented
  = Assignment $ Map.fromListWith Monoid.mappend orderedAssignments
  where orderedAssignments
          = Monoid.mappend (concat untilLast) lastAssignment
        ((maybeLast, afterLast), untilLast)
          = Traversable.mapAccumL assign (Nothing, orderedComments)
              orderedPortions
        assign (maybeLower, rest) upper = ((Just upper, rest'), bindings)
          where (after, before, rest') = assignNext upper rest
                bindings
                  = [(Maybe.fromMaybe upper maybeLower, after), (upper, before)]
        (orderedPortions, orderedComments) = orderedByStartEnd commented
        lastAssignment
          = case maybeLast of
                Nothing -> []
                Just lastPortion -> [(lastPortion, after)]
                  where after = Abstract.createAnnotation [] boxesAfter
                        boxesAfter = createBoxes afterLast

assignNext ::
           Location.SrcSpan ->
             [Source.Comment] ->
               (Abstract.Annotation, Abstract.Annotation, [Source.Comment])
assignNext portion orderedComments = (after, before, rest)
  where after = Abstract.createAnnotation [] boxesAfter
        (boxesAfter, _, boxesBefore) = divideBoxes boxes
        boxes = createBoxes untilPortion
        (untilPortion, rest)
          = span ((<= portion) . Location.getPortion) orderedComments
        before = Abstract.createAnnotation boxesBefore []

divideBoxes ::
            [Abstract.Box] -> ([Abstract.Box], [Abstract.Box], [Abstract.Box])
divideBoxes = divide [] []
  where divide after spaces [] = (after, spaces, [])
        divide after spaces
          rest@(commentBox@(Abstract.CommentBox comment) : unwrapped)
          = case (after, spaces) of
                (_ : _, []) -> ifAfter
                _ -> case displacement of
                         Comment.BeforeElement -> ifBefore
                         Comment.AfterElement -> ifAfter
                         Comment.None -> ifBefore
          where ifAfter
                  = divide (Monoid.mconcat [after, spaces, [commentBox]]) []
                      unwrapped
                displacement = Comment.annotationDisplacement core
                core = Abstract.commentCore comment
                ifBefore = (after, spaces, rest)
        divide after spaces (Abstract.EmptyLine : unwrapped)
          = divide after (Monoid.mappend spaces [Abstract.EmptyLine])
              unwrapped

createBoxes :: [Source.Comment] -> [Abstract.Box]
createBoxes = concat . snd . Traversable.mapAccumL create Nothing
  where create maybeEndLine concreteComment = (Just endLine', boxes)
          where endLine' = Location.getEndLine portion
                portion = Location.getPortion concreteComment
                boxes = Monoid.mappend emptyLines commentBoxes
                emptyLines
                  = case maybeEndLine of
                        Nothing -> []
                        Just endLine -> [Abstract.EmptyLine | lineDistance > 1]
                          where lineDistance = Location.minus startLine endLine :: Integer
                startLine = Location.getStartLine portion
                commentBoxes = [Abstract.CommentBox abstractComment]
                abstractComment = Abstract.createComment Location.base commentCore
                commentCore = Source.commentCore concreteComment

orderedByStartEnd ::
                  Concrete.Commented -> ([Location.SrcSpan], [Source.Comment])
orderedByStartEnd commented = (orderedPortions, orderedComments)
  where orderedPortions = fmap Location.getPortion nestedPortions
        nestedPortions = List.sort $ Foldable.toList root
        root = Concrete.commentedRoot commented
        orderedComments
          = List.sortBy (Function.on compare Location.getPortion) comments
        comments = Concrete.comments commented
