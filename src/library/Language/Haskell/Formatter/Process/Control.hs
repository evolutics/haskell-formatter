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
import qualified Language.Haskell.Formatter.Style as Style

format ::
       Style.Style -> ExactCode.ExactCode -> Result.Result ExactCode.ExactCode
format style = Formatter.format $ createFormatter style

createFormatter :: Style.Style -> Formatter.Formatter
createFormatter style
  = Formatter.Formatter{Formatter.attachComments =
                          AttachComments.attachComments style,
                        Formatter.formatActualCode =
                          FormatActualCode.formatActualCode style,
                        Formatter.formatComments =
                          FormatComments.formatComments style,
                        Formatter.detachComments =
                          DetachComments.detachComments style}
