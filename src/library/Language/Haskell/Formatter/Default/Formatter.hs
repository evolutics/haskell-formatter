module Language.Haskell.Formatter.Default.Formatter (create) where
import qualified
       Language.Haskell.Formatter.Default.CommentAssignment
       as CommentAssignment
import qualified
       Language.Haskell.Formatter.Default.CommentFormatting
       as CommentFormatting
import qualified
       Language.Haskell.Formatter.Default.CommentIntegration
       as CommentIntegration
import qualified
       Language.Haskell.Formatter.Default.ElementArrangement
       as ElementArrangement
import qualified Language.Haskell.Formatter.Formatter as Formatter

create :: Formatter.Formatter
create
  = Formatter.Formatter{Formatter.assignComments =
                          CommentAssignment.assignComments,
                        Formatter.arrangeElements = ElementArrangement.arrangeElements,
                        Formatter.formatComments = CommentFormatting.formatComments,
                        Formatter.integrateComments = CommentIntegration.integrateComments}
