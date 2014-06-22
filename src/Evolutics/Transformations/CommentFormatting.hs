module Evolutics.Transformations.CommentFormatting (formatComments)
       where
import qualified Evolutics.Code.Merged as Merged

formatComments :: Merged.Code -> Merged.Code
formatComments = id
