module Evolutics.Transformations.CommentFormatting (formatComments)
       where
import qualified Data.Function as Function
import qualified Evolutics.Code.Abstract as Abstract
import qualified Evolutics.Code.Core as Core
import qualified Evolutics.Code.Locations as Locations
import qualified Evolutics.Code.Merged as Merged
import qualified Evolutics.Tools.Functions as Functions

formatComments :: Merged.Code -> Merged.Code
formatComments merged = merged{Merged.codeRoot = root'}
  where (_, root')
          = Functions.mapAccummulateLeft1 indent Locations.startPosition root
        indent indentation part
          = (indentation', part{Merged.partAnnotation = annotation'})
          where indentation'
                  = if Function.on (==) Locations.startLine partStart indentation
                      then indentation else partStart
                partStart = Locations.startPosition part
                annotation'
                  = Abstract.mapCommentStartColumns mapStartColumn annotation
                mapStartColumn = const $ Locations.startColumn indentation'
                annotation = Merged.partAnnotation part
        root = Merged.codeRoot merged
