{-|
Description : Parametrization of formatting
-}
module Language.Haskell.Formatter.Style
       (Style(..), Indentation, defaultStyle, check) where
import qualified Data.Maybe as Maybe
import qualified Language.Haskell.Formatter.Error as Error
import qualified Language.Haskell.Formatter.Internal.Newline as Newline
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source

data Style = Style{lineLengthLimit :: Int, ribbonsPerLine :: Float,
                   classIndentation :: Indentation,
                   doIndentation :: Indentation, caseIndentation :: Indentation,
                   letIndentation :: Indentation,
                   whereIndentation :: Indentation,
                   onsideIndentation :: Indentation,
                   orderImportDeclarations :: Bool}
           deriving (Eq, Ord, Show)

data Check = Check (Maybe String)
           deriving (Eq, Ord, Show)

{-| Number of characters used to indent. -}
type Indentation = Int

defaultStyle :: Style
defaultStyle
  = Style{lineLengthLimit = 80, ribbonsPerLine = 1,
          classIndentation = Source.classIndent mode,
          doIndentation = Source.doIndent mode,
          caseIndentation = Source.caseIndent mode,
          letIndentation = Source.letIndent mode,
          whereIndentation = Source.whereIndent mode,
          onsideIndentation = Source.onsideIndent mode,
          orderImportDeclarations = True}
  where mode = Source.defaultMode

check :: Style -> Result.Result ()
check style
  = case maybeError of
        Nothing -> return ()
        Just message -> Result.fatalError $ Error.createStyleFormatError message
  where maybeError
          = case errorMessages of
                [] -> Nothing
                messages -> Just $ Newline.joinSeparatedLines messages
        errorMessages = Maybe.mapMaybe unwrap $ createChecks style
        unwrap (Check errorMessage) = errorMessage

createChecks :: Style -> [Check]
createChecks style
  = concat
      [[lineLengthLimitCheck, ribbonsPerLineCheck], indentationChecks,
       [onsideLessCheck]]
  where lineLengthLimitCheck
          = createCheck (rawLineLengthLimit > 0)
              ["The line length limit must be positive, but it is ",
               show rawLineLengthLimit, "."]
        rawLineLengthLimit = lineLengthLimit style
        ribbonsPerLineCheck
          = createCheck (rawRibbonsPerLine >= 1)
              ["The ribbons per line ratio must be at least 1, but it is ",
               show rawRibbonsPerLine, "."]
        rawRibbonsPerLine = ribbonsPerLine style
        indentationChecks = fmap checkIndentation indentations
        checkIndentation (indentation, name)
          = createCheck (indentation > 0)
              ["The ", name, " indentation must be positive, but it is ",
               show indentation, "."]
        indentations
          = [(rawClassIndentation, "class"), (rawDoIndentation, "do"),
             (rawCaseIndentation, "case"), (rawLetIndentation, "let"),
             (rawWhereIndentation, "where"), (rawOnsideIndentation, onsideName)]
        rawClassIndentation = classIndentation style
        rawDoIndentation = doIndentation style
        rawCaseIndentation = caseIndentation style
        rawLetIndentation = letIndentation style
        rawWhereIndentation = whereIndentation style
        rawOnsideIndentation = onsideIndentation style
        onsideName = "onside"
        onsideLessCheck
          = createCheck
              (and $ fmap (> rawOnsideIndentation) greaterOnsideIndentations)
              ["The ", onsideName,
               " indentation must be less than the other indentations, ",
               "but it is ", show rawOnsideIndentation, "."]
        greaterOnsideIndentations
          = [rawClassIndentation, rawDoIndentation, rawCaseIndentation,
             rawLetIndentation, rawWhereIndentation]

createCheck :: Bool -> [String] -> Check
createCheck False = Check . Just . concat
createCheck True = const $ Check Nothing
