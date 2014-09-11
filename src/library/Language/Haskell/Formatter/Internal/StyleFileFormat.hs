{-|
Description : Sample format definition for a style file
-}
module Language.Haskell.Formatter.Internal.StyleFileFormat (treeFormat) where
import qualified Data.Map as Map
import qualified Language.Haskell.Formatter as Formatter
import qualified Language.Haskell.Formatter.Internal.MapTree as MapTree
import qualified Language.Haskell.Formatter.Internal.TreeFormat as TreeFormat

treeFormat :: TreeFormat.TreeFormat Formatter.Style
treeFormat
  = Map.fromList
      [("line_length_limit",
        MapTree.Leaf . TreeFormat.LimitedInteger $
          \ value style -> style{Formatter.lineLengthLimit = value}),
       ("ribbons_per_line",
        MapTree.Leaf . TreeFormat.SingleFloating $
          \ value style -> style{Formatter.ribbonsPerLine = value}),
       ("indentations",
        MapTree.Node $
          Map.fromList
            [("class",
              MapTree.Leaf . TreeFormat.LimitedInteger $
                \ value style -> style{Formatter.classIndentation = value}),
             ("do",
              MapTree.Leaf . TreeFormat.LimitedInteger $
                \ value style -> style{Formatter.doIndentation = value}),
             ("case",
              MapTree.Leaf . TreeFormat.LimitedInteger $
                \ value style -> style{Formatter.caseIndentation = value}),
             ("let",
              MapTree.Leaf . TreeFormat.LimitedInteger $
                \ value style -> style{Formatter.letIndentation = value}),
             ("where",
              MapTree.Leaf . TreeFormat.LimitedInteger $
                \ value style -> style{Formatter.whereIndentation = value}),
             ("onside",
              MapTree.Leaf . TreeFormat.LimitedInteger $
                \ value style -> style{Formatter.onsideIndentation = value})]),
       ("order",
        MapTree.Node $
          Map.fromList
            [("import_declarations",
              MapTree.Leaf . TreeFormat.Boolean $
                \ value style ->
                  style{Formatter.orderImportDeclarations = value}),
             ("import_entities",
              MapTree.Leaf . TreeFormat.Boolean $
                \ value style ->
                  style{Formatter.orderImportEntities = value})])]
