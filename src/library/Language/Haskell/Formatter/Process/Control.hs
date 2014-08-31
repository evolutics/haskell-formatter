{-|
Description : Formatting process itself
-}
module Language.Haskell.Formatter.Process.Control (format) where
import qualified Language.Haskell.Formatter.ExactCode as ExactCode
import qualified Language.Haskell.Formatter.Process.AttachComments
       as AttachComments
import qualified Language.Haskell.Formatter.Process.DetachComments
       as DetachComments
import qualified Language.Haskell.Formatter.Process.FormatActualCode
       as FormatActualCode
import qualified Language.Haskell.Formatter.Process.FormatComments
       as FormatComments
import qualified Language.Haskell.Formatter.Process.Formatter as Formatter
import qualified Language.Haskell.Formatter.Result as Result

format :: ExactCode.ExactCode -> Result.Result ExactCode.ExactCode
format = Formatter.format createFormatter

createFormatter :: Formatter.Formatter
createFormatter
  = Formatter.Formatter{Formatter.attachComments =
                          AttachComments.attachComments,
                        Formatter.formatActualCode =
                          FormatActualCode.formatActualCode,
                        Formatter.formatComments =
                          FormatComments.formatComments,
                        Formatter.detachComments =
                          DetachComments.detachComments}
