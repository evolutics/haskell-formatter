{-|
Description : Result with possible errors
-}
module Language.Haskell.Formatter.Result
       (Result, toEither, fatalError, fatalAssertionError) where
import qualified Control.Applicative as Applicative
import qualified Control.Monad as Monad
import qualified Language.Haskell.Formatter.Error as Error

newtype Result a = Result (Either Error.Error a)
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

fatalAssertionError :: String -> Result a
fatalAssertionError = fatalError . Error.createAssertionError
