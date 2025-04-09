module Diff.RichTextAnalysis (FormattedCharacter (..), FormattedToken (..), textSpanToFormattedText, tokenizeFormattedText) where

import Data.Char (isSpace)
import qualified Data.Text as T
import DocTree.Common (Mark (..), TextSpan (..))

data FormattedCharacter = FormattedCharacter {char :: Char, charMarks :: [Mark]} deriving (Show, Eq, Ord)

data FormattedToken = FormattedToken {tokenText :: T.Text, tokenChars :: [FormattedCharacter]} deriving (Show, Eq, Ord)

textSpanToFormattedText :: TextSpan -> [FormattedCharacter]
textSpanToFormattedText textSpan = map (\c -> FormattedCharacter c (marks textSpan)) $ T.unpack (value textSpan)

tokenizeFormattedText :: [FormattedCharacter] -> [FormattedToken]
tokenizeFormattedText = map createToken . groupBySpacesAndFormatting
  where
    groupBySpacesAndFormatting :: [FormattedCharacter] -> [[FormattedCharacter]]
    groupBySpacesAndFormatting [] = []
    groupBySpacesAndFormatting (x : xs) = (x : group) : groupBySpacesAndFormatting rest
      where
        (group, rest) = span (isSameFormattingOrSpace x) xs

    isSameFormattingOrSpace :: FormattedCharacter -> FormattedCharacter -> Bool
    isSameFormattingOrSpace fc1 fc2 =
      isSameFormatting fc1 fc2 || isSpaceChar fc1 == isSpaceChar fc2

    -- Check if two characters have the same formatting
    isSameFormatting :: FormattedCharacter -> FormattedCharacter -> Bool
    isSameFormatting fc1 fc2 = charMarks fc1 == charMarks fc2

    isSpaceChar :: FormattedCharacter -> Bool
    isSpaceChar fc = isSpace (char fc)

    -- Helper function to create a FormattedToken from a group of characters
    createToken :: [FormattedCharacter] -> FormattedToken
    createToken charsGroup = FormattedToken (T.pack (map char charsGroup)) charsGroup