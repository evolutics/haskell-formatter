{-|
Description : Syntax tree types
-}
module Language.Haskell.Formatter.Process.Code
       (LocatableCode, CommentableCode, LocatableCommentableCode, tryZipCode,
        tryZipLocationsComments, dropComments, dropLocations)
       where
import qualified Language.Haskell.Formatter.Location as Location
import qualified Language.Haskell.Formatter.Process.Note as Note
import qualified Language.Haskell.Formatter.Result as Result
import qualified Language.Haskell.Formatter.Source as Source
import qualified Language.Haskell.Formatter.Toolkit.Visit as Visit

type LocatableCode = Source.Module Location.SrcSpanInfo

type CommentableCode = Source.Module Note.CommentNote

type LocatableCommentableCode = Source.Module Note.LocationCommentNote

tryZipCode ::
           (a -> b -> c) ->
             Source.Module a ->
               Source.Module b -> Result.Result (Source.Module c)
tryZipCode merge left right
  = case maybeZipped of
        Nothing -> Result.fatalAssertionError message
          where message = "The code notes could not be zipped."
        Just zipped -> return zipped
  where maybeZipped
          = if isActualCodeSame then Visit.halfZipWith merge left right else
              Nothing
        isActualCodeSame = left Source.=~= right

tryZipLocationsComments ::
                        LocatableCode ->
                          CommentableCode ->
                            Result.Result LocatableCommentableCode
tryZipLocationsComments = tryZipCode Note.createLocationCommentNote

dropComments :: LocatableCommentableCode -> LocatableCode
dropComments = fmap Note.locationNote

dropLocations :: LocatableCommentableCode -> CommentableCode
dropLocations = fmap Note.commentNote
