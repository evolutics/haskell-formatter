{-|
Description : Rearranging the actual code (not the comments)
-}
module Language.Haskell.Formatter.Process.FormatActualCode (formatActualCode)
       where
import qualified Control.Applicative as Applicative
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Style as Style
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

formatActualCode ::
                 Style.Style ->
                   Code.LocatableCommentableCode ->
                     Result.Result Code.LocatableCode
formatActualCode style locatableCommentable
  = case parseResult of
        Source.ParseFailed _ _ -> Result.fatalAssertionError message
          where message = "Formatting the actual code failed to parse."
        Source.ParseOk possiblyChanged -> tryUnwrap maybeLocatable'
          where maybeLocatable'
                  = Visit.halfZipWith (flip const) locatable possiblyChanged
  where parseResult = Source.parseFileContents $ prettyPrint style locatable
        locatable = Code.dropComments locatableCommentable
        tryUnwrap maybeLocatable'
          = case maybeLocatable' of
                Nothing -> Result.fatalAssertionError message
                  where message = "Formatting the actual code failed to zip."
                Just locatable' -> return locatable'

prettyPrint :: (Source.Pretty a) => Style.Style -> a -> String
prettyPrint = Applicative.liftA2 Source.prettyPrintStyleMode renderingStyle mode

renderingStyle :: Style.Style -> Source.Style
renderingStyle style
  = Source.style{Source.lineLength = Style.lineLengthLimit style,
                 Source.ribbonsPerLine = Style.ribbonsPerLine style}

mode :: Style.Style -> Source.PPHsMode
mode style
  = Source.defaultMode{Source.classIndent = Style.classIndentation style,
                       Source.doIndent = Style.doIndentation style,
                       Source.caseIndent = Style.caseIndentation style,
                       Source.letIndent = Style.letIndentation style,
                       Source.whereIndent = Style.whereIndentation style,
                       Source.onsideIndent = Style.onsideIndentation style}
