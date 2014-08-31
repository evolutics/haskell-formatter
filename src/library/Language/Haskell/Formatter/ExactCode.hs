{-|
Description : Exact syntax tree as parsed and printed by HSE
-}
module Language.Haskell.Formatter.ExactCode
       (ExactCode, actualCode, comments, create) where
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Source as Source

data ExactCode = ExactCode{actualCode :: Source.Module Location.SrcSpanInfo,
                           comments :: [Source.Comment]}

instance Show ExactCode where
        show exact = Source.exactPrint rawActualCode rawComments
          where rawActualCode = actualCode exact
                rawComments = comments exact

create :: Source.Module Location.SrcSpanInfo -> [Source.Comment] -> ExactCode
create rawActualCode rawComments
  = ExactCode{actualCode = rawActualCode, comments = rawComments}
