module Diff.RichTextAnalysis (FormattedCharacter (..), FormattedToken (..), textSpanToFormattedText, tokenizeFormattedText) where

import Data.Char (isSpace)
import qualified Data.Text as T
import DocTree.Common (Mark (..), TextSpan (..))

data FormattedCharacter = FormattedCharacter {char :: Char, charMarks :: [Mark]} deriving (Show, Eq, Ord)

data FormattedToken = FormattedToken {tokenText :: T.Text, tokenChars :: [FormattedCharacter]} deriving (Show, Eq, Ord)

textSpanToFormattedText :: TextSpan -> [FormattedCharacter]
textSpanToFormattedText textSpan = map (\c -> FormattedCharacter c (marks textSpan)) $ T.unpack (value textSpan)

tokenizeFormattedText :: [FormattedCharacter] -> [FormattedToken]
tokenizeFormattedText = map createToken . groupBySpaces
  where
    groupBySpaces :: [FormattedCharacter] -> [[FormattedCharacter]]
    groupBySpaces [] = []
    groupBySpaces (x : xs) = (x : group) : groupBySpaces rest
      where
        (group, rest) = span (isFormattedSpace x) xs

    isFormattedSpace :: FormattedCharacter -> FormattedCharacter -> Bool
    isFormattedSpace fc1 fc2 = isSpaceChar fc1 == isSpaceChar fc2

    isSpaceChar :: FormattedCharacter -> Bool
    isSpaceChar fc = isSpace (char fc)

    -- Helper function to create a FormattedToken from a group of characters
    createToken :: [FormattedCharacter] -> FormattedToken
    createToken charsGroup = FormattedToken (T.pack (map char charsGroup)) charsGroup