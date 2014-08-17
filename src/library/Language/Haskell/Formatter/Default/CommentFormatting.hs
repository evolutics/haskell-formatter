module Language.Haskell.Formatter.Default.CommentFormatting
       (formatComments) where
import qualified Data.Function as Function
import qualified Language.Haskell.Formatter.Code.Abstract
       as Abstract
import qualified Language.Haskell.Formatter.Code.Location
       as Location
import qualified Language.Haskell.Formatter.Code.Merged as Merged
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

formatComments :: Merged.Code -> Merged.Code
formatComments merged = merged{Merged.codeRoot = root'}
  where (_, root')
          = Visit.mapAccumulateLeftWithCreation indent startPosition root
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
