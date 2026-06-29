module RichTextAnalysis (FormattedCharacter (..), FormattedTextToken (..), InlineAtom (..), InlineToken (..), NoteRefAtom (..), NoteRefToken (..), textSpanToFormattedText, tokenizeInlineSequence) where

import Data.Char (isAlphaNum, isSpace)
import Data.List (groupBy, unfoldr)
import qualified Data.Text as T
import DocTree.Common (Image, Mark (..), NoteId, TextSpan (..))

newtype NoteRefAtom = NoteRefAtom NoteId deriving (Show, Eq, Ord)

data InlineAtom = CharacterAtom FormattedCharacter | NoteAtom NoteRefAtom | ImageAtom Image

isCharAtom :: InlineAtom -> Bool
isCharAtom (CharacterAtom _) = True
isCharAtom _ = False

data FormattedCharacter = FormattedCharacter {char :: Char, charMarks :: [Mark]} deriving (Show, Eq, Ord)

data FormattedTextToken = FormattedTextToken {tokenText :: T.Text, tokenChars :: [FormattedCharacter]} deriving (Show, Eq, Ord)

newtype NoteRefToken = NoteRefToken NoteId deriving (Show, Eq, Ord)

data InlineToken = TextToken FormattedTextToken | NoteToken NoteRefToken | ImageToken Image deriving (Show, Eq, Ord)

textSpanToFormattedText :: TextSpan -> [FormattedCharacter]
textSpanToFormattedText textSpan = map (\c -> FormattedCharacter c (marks textSpan)) $ T.unpack (value textSpan)

data CharType = Whitespace | Alphanumeric | Punctuation deriving (Eq)

charType :: Char -> CharType
charType c
  | isSpace c = Whitespace
  | isAlphaNum c = Alphanumeric
  | otherwise = Punctuation

-- TODO: Explore splitting in note refs **and** spaces with one pass while maintaining clarity.
tokenizeInlineSequence :: [InlineAtom] -> [InlineToken]
tokenizeInlineSequence = retokenizeText . splitInNonTextAtoms
  where
    splitInNonTextAtoms :: [InlineAtom] -> [InlineToken]
    splitInNonTextAtoms = unfoldr emitToken
      where
        emitToken :: [InlineAtom] -> Maybe (InlineToken, [InlineAtom])
        emitToken [] = Nothing
        emitToken ((NoteAtom (NoteRefAtom noteId)) : xs) = Just (NoteToken $ NoteRefToken noteId, xs)
        emitToken ((ImageAtom img) : xs) = Just (ImageToken img, xs)
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

        tokenizeFormattedText :: [FormattedCharacter] -> [InlineToken]
        tokenizeFormattedText = fmap TextToken . map createFormattedTextToken . splitTokens

        -- Split a character run into tokens: maximal runs of one character type, with interior
        -- punctuation re-glued so only leading/trailing punctuation is peeled off.
        splitTokens :: [FormattedCharacter] -> [[FormattedCharacter]]
        splitTokens = glueInteriorPunctuation . groupBy sameType
          where
            sameType :: FormattedCharacter -> FormattedCharacter -> Bool
            sameType a b = charType (char a) == charType (char b)

            -- Glue a punctuation run flanked by two alphanumeric runs back into one word, so
            -- interior punctuation stays attached: "peer","-","to","-","peer" -> "peer-to-peer".
            glueInteriorPunctuation (word : inner : word' : rest)
              | word `isRunOf` Alphanumeric,
                inner `isRunOf` Punctuation,
                word' `isRunOf` Alphanumeric =
                  glueInteriorPunctuation ((word ++ inner ++ word') : rest)
            glueInteriorPunctuation (run : rest) = run : glueInteriorPunctuation rest
            glueInteriorPunctuation [] = []

            isRunOf :: [FormattedCharacter] -> CharType -> Bool
            isRunOf (c : _) t = charType (char c) == t
            isRunOf [] _ = False

    createFormattedTextToken :: [FormattedCharacter] -> FormattedTextToken
    createFormattedTextToken charsGroup = FormattedTextToken (T.pack (map char charsGroup)) charsGroup
