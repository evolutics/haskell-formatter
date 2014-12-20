{-|
Description : Sorting parts of code where the order is irrelevant
-}
module Language.Haskell.Formatter.Process.CodeOrdering
       (orderImportDeclarations, orderRootImportEntities,
        orderNestedImportEntities)
       where
import qualified Language.Haskell.Exts.Annotated.Syntax as Syntax
import qualified Language.Haskell.Formatter.Process.Code as Code
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

orderImportDeclarations ::
                        Code.LocatableCommentableCode ->
                          Code.LocatableCommentableCode
orderImportDeclarations = replaceImportDeclarations $ Visit.orderByKey key
  where key
          (Syntax.ImportDecl _ moduleName isQualified isWithSource isSafe
             package alias entitiesList)
          = (moduleNameKey moduleName, isQualified, isWithSource, isSafe,
             package, fmap moduleNameKey alias,
             fmap entitiesListKey entitiesList)
        moduleNameKey (Syntax.ModuleName _ name) = name
        entitiesListKey (Syntax.ImportSpecList _ isHiding entities)
          = (isHiding, fmap importEntityKey entities)

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

importEntityKey :: Syntax.ImportSpec a -> [String]
importEntityKey (Syntax.IVar _ _ name) = rootNameKey name
importEntityKey (Syntax.IAbs _ name) = rootNameKey name
importEntityKey (Syntax.IThingAll _ name) = rootNameKey name
importEntityKey (Syntax.IThingWith _ name entities)
  = nameKey name : fmap nestedImportEntityKey entities

rootNameKey :: Syntax.Name a -> [String]
rootNameKey name = [nameKey name]

nameKey :: Syntax.Name a -> String
nameKey (Syntax.Ident _ name) = name
nameKey (Syntax.Symbol _ name) = name

nestedImportEntityKey :: Syntax.CName a -> String
nestedImportEntityKey (Syntax.VarName _ name) = nameKey name
nestedImportEntityKey (Syntax.ConName _ name) = nameKey name

orderRootImportEntities ::
                        Code.LocatableCommentableCode ->
                          Code.LocatableCommentableCode
orderRootImportEntities
  = replaceImportEntities $ Visit.orderByKey importEntityKey

replaceImportEntities ::
                      ([Syntax.ImportSpec a] -> [Syntax.ImportSpec a]) ->
                        Source.Module a -> Source.Module a
replaceImportEntities function
  = replaceImportDeclarations $ fmap replaceDeclaration
  where replaceDeclaration importDeclaration
          = importDeclaration{Syntax.importSpecs = entitiesList'}
          where entitiesList' = fmap replaceList entitiesList
                entitiesList = Syntax.importSpecs importDeclaration
        replaceList (Syntax.ImportSpecList annotation isHiding entities)
          = Syntax.ImportSpecList annotation isHiding entities'
          where entities' = function entities

orderNestedImportEntities ::
                          Code.LocatableCommentableCode ->
                            Code.LocatableCommentableCode
orderNestedImportEntities
  = replaceNestedImportEntities $ Visit.orderByKey nestedImportEntityKey

replaceNestedImportEntities ::
                            ([Syntax.CName a] -> [Syntax.CName a]) ->
                              Source.Module a -> Source.Module a
replaceNestedImportEntities function = replaceImportEntities $ fmap replace
  where replace (Syntax.IThingWith annotation name entities)
          = Syntax.IThingWith annotation name entities'
          where entities' = function entities
        replace entity = entity
