{-|
Description : Handling Unicode newlines
-}
module Language.Haskell.Formatter.Internal.Newline
       (newlines, joinSeparatedLines, splitSeparatedLines) where
import qualified Data.List as List
import qualified Language.Haskell.Formatter.Toolkit.Splitter as Splitter

{-| Unicode newline strings ordered by descending length. This corresponds to
    the set of newlines from
    <http://www.unicode.org/standard/reports/tr13/tr13-5.html>. -}
newlines :: [String]
newlines = ["\CR\LF", "\LF", "\VT", "\FF", "\CR", "\x85", "\x2028", "\x2029"]

{-| Concatenates strings with default newlines @\"\\n\"@ between.

    Unlike 'unlines', this does not append a newline to the last string.

    >>> joinSeparatedLines ["apple", "pine"]
    "apple\npine" -}
joinSeparatedLines :: [String] -> String
joinSeparatedLines = List.intercalate defaultNewline
  where defaultNewline = "\LF"

{-| Breaks a string up into its lines at 'newlines'. The resulting strings do
    not contain 'newlines'.

    Unlike 'lines', this interprets a newline as a separator, not a terminator.
    Thus, if the input string ends with a newline, the output list ends with the
    empty string.

    >>> splitSeparatedLines "0\n1\r2\r\n3\n\r4"
    ["0","1","2","3","","4"]

    prop> last (splitSeparatedLines $ s ++ "\LF") == "" -}
splitSeparatedLines :: String -> [String]
splitSeparatedLines = Splitter.separate newlines
