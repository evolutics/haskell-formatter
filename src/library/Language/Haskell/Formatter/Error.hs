module Language.Haskell.Formatter.Error (Error(..)) where
import qualified Data.Monoid as Monoid
import qualified Language.Haskell.Formatter.Code.Location
       as Location
import qualified Language.Haskell.Formatter.Code.Source as Source

data Error = ParseError Location.SrcLoc String
           | CommentAssignmentAssertion
           | ElementArrangementAssertion
           | MergingAssertion
           | CommentFormattingAssertion
           | CommentIntegrationAssertion
           deriving (Eq, Ord)

instance Show Error where
        show (ParseError position message)
          = Monoid.mconcat [Source.prettyPrint position, separator, message]
          where separator = ": "
        show CommentAssignmentAssertion
          = "Assertion error of comment assignment."
        show ElementArrangementAssertion
          = "Assertion error of element arrangement."
        show MergingAssertion = "Assertion error of merging."
        show CommentFormattingAssertion
          = "Assertion error of comment formatting."
        show CommentIntegrationAssertion
          = "Assertion error of comment integration."
