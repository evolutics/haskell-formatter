module Evolutics.Transformations.CommentAssignment (assignComments)
       where
import qualified Data.Foldable as Foldable
import qualified Data.Function as Function
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Traversable as Traversable
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Comment as Comment
import qualified Evolutics.Code.Concrete as Concrete
import qualified Evolutics.Code.Helper as Helper
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Source as Source

data Assignment = Assignment (Map.Map Location.SrcSpan
                                [Source.Comment])
                deriving (Eq, Show)

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
          where (maybeComments, rest')
                  = Map.updateLookupWithKey remove portion rest
                remove = const . const Nothing
                portion = Location.getPortion nestedPortion
                annotation = createAnnotation portion comments
                comments = Foldable.concat maybeComments
        concreteRoot = Concrete.commentlessRoot commentless

createAnnotation ::
                 Location.SrcSpan -> [Source.Comment] -> Abstract.Annotation
createAnnotation portion = Foldable.foldMap single
  where single concreteComment
          = case Helper.portionDisplacement commentPortion portion of
                LT -> Abstract.createAnnotation [abstractComment] []
                EQ -> Abstract.createAnnotation [abstractComment] []
                GT -> Abstract.createAnnotation [] [abstractComment]
          where commentPortion = Location.getPortion concreteComment
                abstractComment = Abstract.createComment Location.base commentCore
                commentCore = Source.commentCore concreteComment

createAssignment :: Concrete.Commented -> Assignment
createAssignment commented
  = Assignment $ Map.fromListWith (flip (++)) orderedAssignments
  where orderedAssignments = concat untilLast ++ lastAssignment
        ((maybeLast, afterLast), untilLast)
          = List.mapAccumL assign (Nothing, comments) orderedPortions
        assign (maybePredecessor, rest) successor
          = ((Just successor, rest'), bindings)
          where (after, before, rest') = assignNext successor rest
                bindings
                  = case maybePredecessor of
                        Nothing -> [(successor, after ++ before)]
                        Just predecessor -> [(predecessor, after), (successor, before)]
        comments = Concrete.comments commented
        orderedPortions = portionsOrderedByStartEnd commented
        lastAssignment
          = case maybeLast of
                Nothing -> []
                Just lastPortion -> [(lastPortion, afterLast)]

assignNext ::
           Location.SrcSpan ->
             [Source.Comment] ->
               ([Source.Comment], [Source.Comment], [Source.Comment])
assignNext portion comments = (after, before, rest)
  where (after, before) = orderByLocation untilPortion
        (untilPortion, rest)
          = List.partition ((<= portion) . Location.getPortion) comments

orderByLocation ::
                [Source.Comment] -> ([Source.Comment], [Source.Comment])
orderByLocation comments = decide ([], []) ordered
  where decide division [] = division
        decide (after, undecided) rest@(comment : more)
          = case displacement of
                Comment.BeforeElement -> (after, undecided ++ rest)
                Comment.AfterElement -> decide (after ++ extended, []) more
                Comment.None -> decide (after, extended) more
          where displacement
                  = Comment.annotationDisplacement $ Source.commentCore comment
                extended = undecided ++ [comment]
        ordered
          = List.sortBy (Function.on compare Location.getPortion) comments

portionsOrderedByStartEnd ::
                          Concrete.Commented -> [Location.SrcSpan]
portionsOrderedByStartEnd commented
  = map Location.getPortion nestedPortions
  where nestedPortions = List.sort $ Foldable.toList root
        root = Concrete.commentedRoot commented
