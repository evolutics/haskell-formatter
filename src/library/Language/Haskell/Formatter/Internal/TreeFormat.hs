{-|
Description : Parsing nested maps according to a format
-}
module Language.Haskell.Formatter.Internal.TreeFormat
       (TreeFormat, Leaf(..), parseYamlFile) where
import qualified Control.Arrow as Arrow
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map.Strict as Map
import qualified Data.Monoid as Monoid
import qualified Data.Scientific as Scientific
import qualified Data.Text as Text
import qualified Data.Yaml as Yaml
import qualified Language.Haskell.Formatter.Internal.MapTree as MapTree
import qualified Language.Haskell.Formatter.Internal.Newline as Newline

type TreeFormat a = MapTree.MapForest String (Leaf a)

data Leaf a = Boolean (RawLeaf Bool a)
            | LimitedInteger (RawLeaf Int a)
            | SingleFloating (RawLeaf Float a)

type RawLeaf a b = a -> b -> b

parseYamlFile :: TreeFormat a -> a -> FilePath -> IO (Either String a)
parseYamlFile format ball file
  = do maybeValue <- Yaml.decodeFileEither file
       let interpretation
             = case maybeValue of
                   Left exception -> Left $ show exception
                   Right value -> defaultInterpret format value ball
       return $ Arrow.left fileError interpretation
  where fileError message = Newline.joinSeparatedLines [introduction, message]
        introduction = Monoid.mappend file ":"

defaultInterpret :: TreeFormat a -> Yaml.Value -> a -> Either String a
defaultInterpret format value ball
  = if MapTree.isEmpty errors then Right interpretation else
      Left $ MapTree.indentTree errors
  where (errors, interpretation) = interpret format value ball

interpret ::
          TreeFormat a -> Yaml.Value -> a -> (MapTree.MapTree String String, a)
interpret (MapTree.MapForest formatMap) (Yaml.Object rawValueMap) ball
  = (errorNode, ball')
  where errorNode = MapTree.Node . MapTree.MapForest $ Map.mapMaybe id errorTree
        (ball', errorTree) = Map.mapAccumWithKey move ball valueMap
        move ballPart key value = (ballPart', maybeErrors)
          where (maybeErrors, ballPart') = matchTree maybeFormat value ballPart
                maybeFormat = Map.lookup key formatMap
        valueMap = Map.mapKeys Text.unpack $ orderedMap rawValueMap
        orderedMap = Map.fromList . HashMap.toList
interpret _ value ball = (errorLeaf, ball)
  where errorLeaf = MapTree.Leaf $ unexpectedMessage "a map" value

matchTree ::
          Maybe (MapTree.MapTree String (Leaf a)) ->
            Yaml.Value -> a -> (Maybe (MapTree.MapTree String String), a)
matchTree Nothing _ ball = (Just $ MapTree.Leaf message, ball)
  where message = "Unexpected key."
matchTree (Just (MapTree.Leaf leaf)) value ball
  = case matchLeaf leaf value ball of
        Left message -> (Just $ MapTree.Leaf message, ball)
        Right ball' -> (Nothing, ball')
matchTree (Just (MapTree.Node node)) value ball = (maybeErrors, ball')
  where maybeErrors = if MapTree.isEmpty errors then Nothing else Just errors
        (errors, ball') = interpret node value ball

matchLeaf :: Leaf a -> Yaml.Value -> a -> Either String a
matchLeaf (Boolean go) (Yaml.Bool boolean) ball = Right $ go boolean ball
matchLeaf (LimitedInteger go) value@(Yaml.Number number) ball
  = case Scientific.toBoundedInteger number of
        Nothing -> Left message
          where message = unexpectedMessage "a limited integer" value
        Just integer -> Right $ go integer ball
matchLeaf (SingleFloating go) (Yaml.Number number) ball
  = Right $ go floating ball
  where floating = Scientific.toRealFloat number
matchLeaf format value _ = Left $ unexpectedMessage expected value
  where expected
          = case format of
                Boolean _ -> "a Boolean"
                LimitedInteger _ -> "a limited integer"
                SingleFloating _ -> "a single-precision floating-point number"

unexpectedMessage :: String -> Yaml.Value -> String
unexpectedMessage expected actualValue
  = Newline.joinSeparatedLines [introduction, actual]
  where introduction = concat ["Expected ", expected, ", but got:"]
        actual = show $ Yaml.encode actualValue
