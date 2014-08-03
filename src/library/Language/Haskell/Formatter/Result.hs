module Language.Haskell.Formatter.Result
       (Result, toEither, fatalError, check, checkMaybe) where
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Language.Haskell.Formatter.Error as Error
import Prelude hiding (error)

data Result a = Result (Either Error.Error a)
              deriving (Eq, Ord, Show)

instance Functor Result where
        fmap = Monad.liftM

instance Applicative.Applicative Result where
        pure = return
        (<*>) = Monad.ap

instance Monad Result where
        return = Result . return
        Result result >>= action = Result $ result >>= toEither . action

toEither :: Result a -> Either Error.Error a
toEither (Result result) = result

fatalError :: Error.Error -> Result a
fatalError = Result . Left

check :: Error.Error -> a -> Bool -> Result a
check error success isSuccess
  = checkMaybe error $ if isSuccess then Just success else Nothing

checkMaybe :: Error.Error -> Maybe a -> Result a
checkMaybe error maybeSuccess
  = case maybeSuccess of
        Nothing -> fatalError error
        Just success -> return success
