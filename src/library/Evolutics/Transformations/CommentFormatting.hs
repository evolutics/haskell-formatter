module Evolutics.Transformations.CommentFormatting (formatComments)
       where
import qualified Data.Function as Function
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Location as Location
import qualified Evolutics.Code.Merged as Merged
import qualified Evolutics.Tools.Functions as Functions

formatComments :: Merged.Code -> Merged.Code
formatComments merged = merged{Merged.codeRoot = root'}
  where (_, root')
          = Functions.mapAccumulateLeft1 indent startPosition root
        indent indentation part
          = (indentation', part{Merged.partAnnotation = annotation'})
          where indentation'
                  = if Function.on (==) Location.getStartLine partStart indentation
                      then indentation else partStart
                partStart = startPosition part
                annotation'
                  = Abstract.mapCommentStartColumns mapStartColumn annotation
                mapStartColumn = const $ Location.getStartColumn indentation'
                annotation = Merged.partAnnotation part
        startPosition = Location.getPointLoc . Location.getPortion
        root = Merged.codeRoot merged
