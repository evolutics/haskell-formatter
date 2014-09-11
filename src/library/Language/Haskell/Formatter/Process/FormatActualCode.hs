{-|
Description : Rearranging the actual code (not the comments)
-}
module Language.Haskell.Formatter.Process.FormatActualCode (formatActualCode)
       where
import qualified Control.Applicative as Applicative
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Style as Style
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

formatActualCode ::
                 Style.Style ->
                   Code.LocatableCommentableCode ->
                     Result.Result Code.LocatableCommentableCode
formatActualCode style locatableCommentable
  = do locatable <- prettyPrint style locatableCommentable'
       Code.tryZipLocationsComments locatable commentable
  where locatableCommentable' = prepare style locatableCommentable
        commentable = Code.dropLocations locatableCommentable'

prepare ::
        Style.Style ->
          Code.LocatableCommentableCode -> Code.LocatableCommentableCode
prepare style = Visit.compose preparations
  where preparations
          = [preparation | (isApplied, preparation) <- applications,
             isApplied style]
        applications
          = [(Style.orderImportDeclarations, orderImportDeclarations)]

orderImportDeclarations ::
                        Code.LocatableCommentableCode ->
                          Code.LocatableCommentableCode
orderImportDeclarations = replaceImportDeclarations replace
  where replace = Visit.orderByKey key
        key
          (Syntax.ImportDecl _ moduleName isQualified isWithSource package alias
             entitiesList)
          = (moduleNameKey moduleName, isQualified, isWithSource, package,
             fmap moduleNameKey alias, fmap entitiesListKey entitiesList)
        moduleNameKey (Syntax.ModuleName _ name) = name
        entitiesListKey (Syntax.ImportSpecList _ isHiding entities)
          = (isHiding, fmap entityKey entities)

replaceImportDeclarations ::
                          ([Syntax.ImportDecl a] -> [Syntax.ImportDecl a]) ->
                            Source.Module a -> Source.Module a
replaceImportDeclarations function (Syntax.Module a h p importDeclarations d)
  = Syntax.Module a h p importDeclarations' d
  where importDeclarations' = function importDeclarations
replaceImportDeclarations _ xmlPage@(Syntax.XmlPage _ _ _ _ _ _ _) = xmlPage
replaceImportDeclarations function
  (Syntax.XmlHybrid a h p importDeclarations d xn xa me e)
  = Syntax.XmlHybrid a h p importDeclarations' d xn xa me e
  where importDeclarations' = function importDeclarations

entityKey :: Syntax.ImportSpec a -> [String]
entityKey (Syntax.IVar _ name) = rootNameKey name
entityKey (Syntax.IAbs _ name) = rootNameKey name
entityKey (Syntax.IThingAll _ name) = rootNameKey name
entityKey (Syntax.IThingWith _ name nestedEntities)
  = nameKey name : fmap nestedEntityKey nestedEntities

rootNameKey :: Syntax.Name a -> [String]
rootNameKey name = [nameKey name]

nameKey :: Syntax.Name a -> String
nameKey (Syntax.Ident _ name) = name
nameKey (Syntax.Symbol _ name) = name

nestedEntityKey :: Syntax.CName a -> String
nestedEntityKey (Syntax.VarName _ name) = nameKey name
nestedEntityKey (Syntax.ConName _ name) = nameKey name

prettyPrint ::
            Style.Style ->
              Code.LocatableCommentableCode -> Result.Result Code.LocatableCode
prettyPrint style locatableCommentable
  = case parseResult of
        Source.ParseFailed _ _ -> Result.fatalAssertionError message
          where message = "Formatting the actual code failed to parse."
        Source.ParseOk possiblyChanged -> tryUnwrap maybeLocatable'
          where maybeLocatable'
                  = Visit.halfZipWith (flip const) locatable possiblyChanged
  where parseResult
          = Source.parseFileContents $ defaultPrettyPrint style locatable
        locatable = Code.dropComments locatableCommentable
        tryUnwrap maybeLocatable'
          = case maybeLocatable' of
                Nothing -> Result.fatalAssertionError message
                  where message = "Formatting the actual code failed to zip."
                Just locatable' -> return locatable'

defaultPrettyPrint :: (Source.Pretty a) => Style.Style -> a -> String
defaultPrettyPrint
  = Applicative.liftA2 Source.prettyPrintStyleMode renderingStyle mode

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
