module RichTextAnalysis (FormattedCharacter (..), FormattedTextToken (..), InlineAtom (..), InlineToken (..), textSpanToFormattedText, tokenizeInlineSequence) where

import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Text as T
import DocTree.Common (Mark (..), NoteId, TextSpan (..))

data InlineAtom = CharacterAtom FormattedCharacter | NoteRefAtom NoteId

isCharAtom :: InlineAtom -> Bool
isCharAtom (CharacterAtom _) = True
isCharAtom _ = False

data FormattedCharacter = FormattedCharacter {char :: Char, charMarks :: [Mark]} deriving (Show, Eq, Ord)

data FormattedTextToken = FormattedTextToken {tokenText :: T.Text, tokenChars :: [FormattedCharacter]} deriving (Show, Eq, Ord)

data InlineToken = TextToken FormattedTextToken | NoteRefToken NoteId deriving (Show, Eq, Ord)

textSpanToFormattedText :: TextSpan -> [FormattedCharacter]
textSpanToFormattedText textSpan = map (\c -> FormattedCharacter c (marks textSpan)) $ T.unpack (value textSpan)

-- TODO: Explore splitting in note refs **and** spaces with one pass while maintaining clarity.
tokenizeInlineSequence :: [InlineAtom] -> [InlineToken]
tokenizeInlineSequence = retokenizeText . splitInNoteRefs
  where
    splitInNoteRefs :: [InlineAtom] -> [InlineToken]
    splitInNoteRefs = unfoldr emitToken
      where
        emitToken :: [InlineAtom] -> Maybe (InlineToken, [InlineAtom])
        emitToken [] = Nothing
        emitToken (NoteRefAtom noteId : xs) = Just (NoteRefToken noteId, xs)
        emitToken (charAtom@(CharacterAtom _) : xs) = Just (TextToken $ createFormattedTextToken consecutiveChars, rest)
          where
            (consecutiveCharAtoms, rest) = span isCharAtom (charAtom : xs)
            consecutiveChars = [fc | CharacterAtom fc <- consecutiveCharAtoms]

    retokenizeText :: [InlineToken] -> [InlineToken]
    retokenizeText = concatMap retokenizeTextToken
      where
        retokenizeTextToken :: InlineToken -> [InlineToken]
        retokenizeTextToken (TextToken textToken) = tokenizeFormattedText $ tokenChars textToken
        retokenizeTextToken otherToken = [otherToken]

        -- TODO: See if we can refactor this part to use library functions like `words`.
        tokenizeFormattedText :: [FormattedCharacter] -> [InlineToken]
        tokenizeFormattedText = fmap TextToken . map createFormattedTextToken . groupBySpaces

        groupBySpaces :: [FormattedCharacter] -> [[FormattedCharacter]]
        groupBySpaces [] = []
        groupBySpaces (x : xs) = (x : group) : groupBySpaces rest
          where
            (group, rest) = span (isFormattedSpace x) xs

        isFormattedSpace :: FormattedCharacter -> FormattedCharacter -> Bool
        isFormattedSpace fc1 fc2 = isSpaceChar fc1 == isSpaceChar fc2

        isSpaceChar :: FormattedCharacter -> Bool
        isSpaceChar fc = isSpace (char fc)

    createFormattedTextToken :: [FormattedCharacter] -> FormattedTextToken
    createFormattedTextToken charsGroup = FormattedTextToken (T.pack (map char charsGroup)) charsGroup
