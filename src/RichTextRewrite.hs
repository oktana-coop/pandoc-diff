module RichTextRewrite (isRewrite) where

import Data.List (groupBy)
import RichTextAnalysis (InlineAtom, isCharAtom)
import RichTextDiffOp (RichTextDiffOp (..), unpackDiffOpValue)

minChangedRegions :: Int
minChangedRegions = 2

minChangedPercent :: Int
minChangedPercent = 70

-- A change is treated as a rewrite when the prose is edited in at least
-- `minChangedRegions` separate places AND more than `minChangedPercent`% of it is changed.
isRewrite :: [RichTextDiffOp InlineAtom] -> Bool
isRewrite inlineDiff =
  countChangedRegions proseAtomOps >= minChangedRegions
    && 100 * changedAtoms > minChangedPercent * totalAtoms
  where
    -- Only prose decides a rewrite; note-reference and image churn (e.g. renumbering footnotes)
    -- must not turn a paragraph whose text is unchanged into a whole replace.
    proseAtomOps = filter (isCharAtom . unpackDiffOpValue) inlineDiff
    totalAtoms = length proseAtomOps
    changedAtoms = length (filter isChanged proseAtomOps)

-- Does this op change the text content? Insert adds a character, Delete removes one -- both change
-- the text. Copy leaves it; a mark update changes formatting but not the characters, so Unchanged.
data TextEffect = Changed | Unchanged deriving (Eq)

textEffect :: RichTextDiffOp a -> TextEffect
textEffect (Insert _) = Changed
textEffect (Delete _) = Changed
textEffect (Copy _) = Unchanged
textEffect (UpdateMeta _ _) = Unchanged
textEffect (UpdateMarks _ _) = Unchanged
textEffect (UpdateHeadingLevel _ _) = Unchanged

isChanged :: RichTextDiffOp a -> Bool
isChanged = (== Changed) . textEffect

-- How many separate places the prose was changed: maximal runs of Changed ops, each delimited by
-- Unchanged ops. A substitution (Delete then Insert) is a single region - only an Unchanged op
-- sitting between edits starts a new one.
countChangedRegions :: [RichTextDiffOp a] -> Int
countChangedRegions = length . filter isChangedRegion . groupBy sameEffect
  where
    sameEffect a b = textEffect a == textEffect b
    isChangedRegion = all isChanged
