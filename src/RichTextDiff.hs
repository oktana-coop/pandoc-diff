{-# LANGUAGE InstanceSigs #-}
-- Disabled orphan instance warning because we must define an `Eq` instance for EditTree.
{-# OPTIONS_GHC -Wno-orphans #-}

module RichTextDiff
  ( RichTextDiff.diff,
    getAnnotatedTree,
  )
where

import Data.List (sort)
import qualified Data.Text as T
import Data.Tree (Tree, drawTree, unfoldTree)
import Data.TreeDiff (Edit (..))
import Data.TreeDiff.Tree (EditTree (..), treeDiff)
import Debug.Trace
import DocTree.Common (Mark (..), NoteId (..), TextSpan (..))
import DocTree.GroupedInlines (BlockNode (..), DocNode (..), InlineNode (..), InlineSpan (..), TreeNode (..), toTree)
import DocTree.LeafTextSpans (DocNode (..), TreeNode (..))
import Patience (Item (..), diff)
import RichTextAnalysis (FormattedCharacter (..), FormattedTextToken (..), InlineAtom (..), InlineToken (..), NoteRefAtom (..), NoteRefToken (..), textSpanToFormattedText, tokenizeInlineSequence)
import RichTextDiffOp (HeadingLevelDiff (..), MarkDiff (..), RichTextDiffOp (..), getDiffOpType, unpackDiffOpValue)
import Text.Pandoc.Definition as Pandoc (Block (Div, Header), Pandoc, nullAttr)

instance (Eq a) => Eq (EditTree a) where
  (EditNode a1 edits1) == (EditNode a2 edits2) = a1 == a2 && edits1 == edits2

-- Helper wrapper type used to:
-- Compare the plain text (ignore formatting) when using the (patience) diff algorithm for characters.
-- Compare the note ID when using the patience diff for note refs.
newtype CompareInlineAtom = CompareInlineAtom InlineAtom

instance Eq CompareInlineAtom where
  (==) :: CompareInlineAtom -> CompareInlineAtom -> Bool
  (CompareInlineAtom (CharacterAtom (FormattedCharacter c1 _))) == (CompareInlineAtom (CharacterAtom (FormattedCharacter c2 _))) = c1 == c2
  (CompareInlineAtom (NoteAtom (NoteRefAtom (NoteId id1)))) == (CompareInlineAtom (NoteAtom (NoteRefAtom (NoteId id2)))) = id1 == id2
  _ == _ = False

-- Helper wrapper type used to compare the plain text (ignore formatting) when using the (patience) diff algorithm for characters.
newtype ComparePlainText = ComparePlainText FormattedCharacter

instance Eq ComparePlainText where
  (==) :: ComparePlainText -> ComparePlainText -> Bool
  (ComparePlainText (FormattedCharacter c1 _)) == (ComparePlainText (FormattedCharacter c2 _)) = c1 == c2

instance Ord ComparePlainText where
  compare :: ComparePlainText -> ComparePlainText -> Ordering
  compare (ComparePlainText a) (ComparePlainText b) = compare (char a) (char b)

newtype CompareTokenText = CompareTokenText FormattedTextToken

-- Helper wrapper type used to compare the plain text (ignore formatting) when using the (patience) diff algorithm for words/tokens.
instance Eq CompareTokenText where
  (==) :: CompareTokenText -> CompareTokenText -> Bool
  (CompareTokenText t1) == (CompareTokenText t2) = tokenText t1 == tokenText t2

instance Ord CompareTokenText where
  compare :: CompareTokenText -> CompareTokenText -> Ordering
  compare (CompareTokenText t1) (CompareTokenText t2) = compare (tokenText t1) (tokenText t2)

newtype CompareInlineToken = CompareInlineToken InlineToken

-- Helper wrapper type used to:
-- Compare the plain text (ignore formatting) when using the (patience) diff algorithm for words/tokens.
-- Compare the note ID when using the patience diff for note ref tokens.
instance Eq CompareInlineToken where
  (CompareInlineToken (TextToken t1)) == (CompareInlineToken (TextToken t2)) = tokenText t1 == tokenText t2
  (CompareInlineToken (NoteToken id1)) == (CompareInlineToken (NoteToken id2)) = id1 == id2
  _ == _ = False

instance Ord CompareInlineToken where
  compare (CompareInlineToken (NoteToken n1)) (CompareInlineToken (NoteToken n2)) = compare n1 n2
  compare (CompareInlineToken (TextToken t1)) (CompareInlineToken (TextToken t2)) = compare (tokenText t1) (tokenText t2)
  -- Define a natural order for different token types. This has to be some type of convention. Here we define that
  compare (CompareInlineToken (NoteToken _)) (CompareInlineToken (TextToken _)) = LT
  compare (CompareInlineToken (TextToken _)) (CompareInlineToken (NoteToken _)) = GT

data EditScript
  = TreeEditScript (Edit (EditTree DocTree.GroupedInlines.DocNode))
  | InlineEditScript (RichTextDiffOp InlineSpan)
  deriving (Show)

traceTree :: (Show a) => Tree a -> Tree a
traceTree tree = Debug.Trace.trace (drawTree $ fmap show tree) tree

diff :: Pandoc.Pandoc -> Pandoc.Pandoc -> Pandoc.Pandoc
diff pandoc1 pandoc2 = (toPandoc . unfoldAnnotatedTreeFromEditScript) editScript
  where
    tree1 = traceTree $ DocTree.GroupedInlines.toTree pandoc1
    tree2 = traceTree $ DocTree.GroupedInlines.toTree pandoc2
    -- Diff the 2 trees and get the edit script
    editScript = TreeEditScript $ treeDiff tree1 tree2

-- TODO: Remove when all the components of the diff function (annotateTreeWithDiffs, toPandoc) are implemented
getAnnotatedTree :: Pandoc.Pandoc -> Pandoc.Pandoc -> Tree (RichTextDiffOp DocTree.LeafTextSpans.DocNode)
getAnnotatedTree pandoc1 pandoc2 = traceTree $ unfoldAnnotatedTreeFromEditScript editScript
  where
    tree1 = traceTree $ DocTree.GroupedInlines.toTree pandoc1
    tree2 = traceTree $ DocTree.GroupedInlines.toTree pandoc2
    -- Diff the 2 trees and get the edit script
    editScript = TreeEditScript $ treeDiff tree1 tree2

-- Produce the annotated tree from the edit script that contains the diffs
unfoldAnnotatedTreeFromEditScript :: EditScript -> Tree (RichTextDiffOp DocTree.LeafTextSpans.DocNode)
unfoldAnnotatedTreeFromEditScript = unfoldTree annotatedTreeNodeUnfolder

toPandoc :: Tree (RichTextDiffOp DocTree.LeafTextSpans.DocNode) -> Pandoc.Pandoc
toPandoc = undefined

annotatedTreeNodeUnfolder :: EditScript -> (RichTextDiffOp DocTree.LeafTextSpans.DocNode, [EditScript])
-- Root node
-- Leave Cpy nodes unchanged. Just return their sub-forest edit scripts as the next seeds to be unfolded.
annotatedTreeNodeUnfolder (TreeEditScript (Cpy (EditNode (DocTree.GroupedInlines.Root) subForestEditScripts))) =
  (Copy DocTree.LeafTextSpans.Root, map TreeEditScript subForestEditScripts)
annotatedTreeNodeUnfolder (TreeEditScript (Ins (EditNode (DocTree.GroupedInlines.Root) subForestEditScripts))) =
  (Insert DocTree.LeafTextSpans.Root, map (TreeEditScript . replaceWithInsOp) subForestEditScripts)
annotatedTreeNodeUnfolder (TreeEditScript (Del (EditNode (DocTree.GroupedInlines.Root) subForestEditScripts))) =
  (Delete DocTree.LeafTextSpans.Root, map (TreeEditScript . replaceWithDelOp) subForestEditScripts)
annotatedTreeNodeUnfolder (TreeEditScript (Swp (EditNode (DocTree.GroupedInlines.Root) subForest1EditScripts) (EditNode (DocTree.GroupedInlines.Root) subForest2EditScripts))) =
  (Copy DocTree.LeafTextSpans.Root, handleSwappedSubForests subForest1EditScripts subForest2EditScripts)
-- Block nodes
-- Leave Cpy nodes unchanged. Just return their sub-forest edit scripts as the next seeds to be unfolded.
annotatedTreeNodeUnfolder (TreeEditScript (Cpy (EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode blockNode)) subForestEditScripts))) =
  (Copy $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.BlockNode blockNode, map TreeEditScript subForestEditScripts)
annotatedTreeNodeUnfolder (TreeEditScript (Ins (EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode blockNode)) subForestEditScripts))) =
  (Insert $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.BlockNode blockNode, map (TreeEditScript . replaceWithInsOp) subForestEditScripts)
annotatedTreeNodeUnfolder (TreeEditScript (Del (EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode blockNode)) subForestEditScripts))) =
  (Delete $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.BlockNode blockNode, map (TreeEditScript . replaceWithDelOp) subForestEditScripts)
-- In this case of swapping headings, check if the only thing that's changed is the heading level, in which case we return the respective diff type.
-- Otherwise, it's regular block swap handling.
annotatedTreeNodeUnfolder
  ( TreeEditScript
      ( Swp
          block1@(EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode (PandocBlock heading1@(Pandoc.Header level1 _ _)))) subForest1EditScripts)
          block2@(EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode (PandocBlock heading2@(Pandoc.Header level2 _ _)))) subForest2EditScripts)
        )
    ) =
    if onlyHeadingLevelsDiffer heading1 heading2 && subForest1EditScripts == subForest2EditScripts
      then (produceHeadingLevelChangeDiffOp heading2 level1 level2, map TreeEditScript subForest2EditScripts)
      else handleSwappedBlocks block1 block2
-- In this case of swapping blocks, we add a wrapper div container to the tree and create del+ins operations for the swapped blocks respectively.
annotatedTreeNodeUnfolder
  ( TreeEditScript
      ( Swp
          block1@(EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode _)) _)
          block2@(EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode _)) _)
        )
    ) = handleSwappedBlocks block1 block2
-- Inline nodes
-- We ignore the subforest edit scripts tree diffing gave us here. Any edit scripts may occur by inline diffing, which is handled by a different algorithm.
annotatedTreeNodeUnfolder (TreeEditScript (Cpy (EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.InlineNode (DocTree.GroupedInlines.InlineContent textSpans))) _))) =
  (Copy $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.InlineNode, map (InlineEditScript . Copy) textSpans)
annotatedTreeNodeUnfolder (TreeEditScript (Ins (EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.InlineNode (DocTree.GroupedInlines.InlineContent textSpans))) _))) =
  (Insert $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.InlineNode, map (InlineEditScript . Insert) textSpans)
annotatedTreeNodeUnfolder (TreeEditScript (Del (EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.InlineNode (DocTree.GroupedInlines.InlineContent textSpans))) _))) =
  (Delete $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.InlineNode, map (InlineEditScript . Delete) textSpans)
-- In the case of swapping inlines, we call `diffInlineNodes` to handle inline node diffing with an algorithm that diffs inline text (not trees).
annotatedTreeNodeUnfolder (TreeEditScript (Swp (EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.InlineNode inlineNode1)) _) (EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.InlineNode inlineNode2)) _))) =
  (Copy $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.InlineNode, diffInlineNodes inlineNode1 inlineNode2)
-- Wrap the diffed text span with the tree node constructors
annotatedTreeNodeUnfolder (InlineEditScript inlineEditScript) =
  (fmap (DocTree.LeafTextSpans.TreeNode . DocTree.LeafTextSpans.InlineContent) inlineEditScript, [])
-- Other cases (taking different types of nodes as input from the edit script)
-- TODO: Here we must return an error because we are in cases where the edit script is wrong (e.g. trying to replace the Root node with another block or inline node).
annotatedTreeNodeUnfolder _ = undefined

-- To handle swapped blocks, add a wrapper div container to the tree and create del+ins operations for the swapped blocks respectively.
handleSwappedBlocks :: EditTree DocTree.GroupedInlines.DocNode -> EditTree DocTree.GroupedInlines.DocNode -> (RichTextDiffOp DocTree.LeafTextSpans.DocNode, [EditScript])
handleSwappedBlocks block1 block2 = (Copy $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.BlockNode $ PandocBlock $ Pandoc.Div nullAttr [], [(TreeEditScript . Del) block1, (TreeEditScript . Ins) block2])

handleSwappedSubForests :: [Edit (EditTree DocTree.GroupedInlines.DocNode)] -> [Edit (EditTree DocTree.GroupedInlines.DocNode)] -> [EditScript]
handleSwappedSubForests deletedSubForests insertedSubForests = (map (TreeEditScript . replaceWithDelOp) deletedSubForests) <> (map (TreeEditScript . replaceWithInsOp) insertedSubForests)

onlyHeadingLevelsDiffer :: Pandoc.Block -> Pandoc.Block -> Bool
onlyHeadingLevelsDiffer (Pandoc.Header level1 attrs1 inlines1) (Pandoc.Header level2 attrs2 inlines2) = level1 /= level2 && attrs1 == attrs2 && inlines1 == inlines2
onlyHeadingLevelsDiffer _ _ = False

produceHeadingLevelChangeDiffOp :: Pandoc.Block -> Int -> Int -> RichTextDiffOp DocTree.LeafTextSpans.DocNode
produceHeadingLevelChangeDiffOp headingBlock l1 l2 = UpdateHeadingLevel (HeadingLevelDiff l1 l2) (DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.BlockNode $ PandocBlock headingBlock)

diffInlineNodes :: DocTree.GroupedInlines.InlineNode -> DocTree.GroupedInlines.InlineNode -> [EditScript]
diffInlineNodes deletedInlineNode addedInlineNode = buildAnnotatedInlineNodeFromDiff $ diffInlineTokens ((tokenizeInlineSequence . toInlineAtoms) deletedInlineNode) ((tokenizeInlineSequence . toInlineAtoms) addedInlineNode)

toInlineAtoms :: DocTree.GroupedInlines.InlineNode -> [InlineAtom]
toInlineAtoms (DocTree.GroupedInlines.InlineContent inlineSpans) = concatMap inlineSpanToInlineAtoms inlineSpans

inlineSpanToInlineAtoms :: InlineSpan -> [InlineAtom]
inlineSpanToInlineAtoms (InlineText textSpan) = map CharacterAtom $ textSpanToFormattedText textSpan
inlineSpanToInlineAtoms (NoteRef (NoteId noteId)) = [NoteAtom $ NoteRefAtom (NoteId noteId)]

diffInlineTokens :: [InlineToken] -> [InlineToken] -> [RichTextDiffOp InlineAtom]
diffInlineTokens tokens1 tokens2 =
  concatMap resolveTokenDiff $
    Patience.diff
      (map CompareInlineToken tokens1)
      (map CompareInlineToken tokens2)
  where
    resolveTokenDiff :: Patience.Item CompareInlineToken -> [RichTextDiffOp InlineAtom]
    -- Text deletions/insertions
    resolveTokenDiff (Patience.Old (CompareInlineToken (TextToken t))) =
      map (Delete . CharacterAtom) (tokenChars t)
    resolveTokenDiff (Patience.New (CompareInlineToken (TextToken t))) =
      map (Insert . CharacterAtom) (tokenChars t)
    resolveTokenDiff (Patience.Both (CompareInlineToken (TextToken t1)) (CompareInlineToken (TextToken t2))) =
      map (fmap CharacterAtom) (diffFormattedTextTokens t1 t2)
    -- Note ref deletions/insertions
    resolveTokenDiff (Patience.Old (CompareInlineToken (NoteToken (NoteRefToken noteId)))) =
      [(Delete . NoteAtom . NoteRefAtom) noteId]
    resolveTokenDiff (Patience.New (CompareInlineToken (NoteToken (NoteRefToken noteId)))) =
      [(Insert . NoteAtom . NoteRefAtom) noteId]
    resolveTokenDiff (Patience.Both (CompareInlineToken (NoteToken t1)) (CompareInlineToken (NoteToken t2))) =
      map (fmap NoteAtom) (diffNoteRefTokens t1 t2)
    -- Mixed-type Both cases (text <-> note ref)
    resolveTokenDiff (Patience.Both (CompareInlineToken (TextToken t)) (CompareInlineToken (NoteToken (NoteRefToken noteId)))) =
      map (Delete . CharacterAtom) (tokenChars t) ++ [(Insert . NoteAtom . NoteRefAtom) noteId]
    resolveTokenDiff (Patience.Both (CompareInlineToken (NoteToken (NoteRefToken noteId))) (CompareInlineToken (TextToken t))) =
      [(Delete . NoteAtom . NoteRefAtom) noteId] ++ map (Insert . CharacterAtom) (tokenChars t)

diffFormattedTextTokens :: FormattedTextToken -> FormattedTextToken -> [RichTextDiffOp FormattedCharacter]
diffFormattedTextTokens t1 t2 =
  if tokenText t1 == tokenText t2
    then diffFormatting t1 t2
    -- Fallback char-by-char diff
    else diffFormattedText (tokenChars t1) (tokenChars t2)
  where
    diffFormatting :: FormattedTextToken -> FormattedTextToken -> [RichTextDiffOp FormattedCharacter]
    diffFormatting (FormattedTextToken _ chars1) (FormattedTextToken _ chars2) = zipWith compareCharFormatting chars1 chars2

    diffFormattedText :: [FormattedCharacter] -> [FormattedCharacter] -> [RichTextDiffOp FormattedCharacter]
    diffFormattedText formattedText1 formattedText2 =
      map compareFormattingForUnchangedChars $ Patience.diff (map ComparePlainText formattedText1) (map ComparePlainText formattedText2)
      where
        compareFormattingForUnchangedChars :: Patience.Item ComparePlainText -> RichTextDiffOp FormattedCharacter
        compareFormattingForUnchangedChars (Patience.Old (ComparePlainText fc)) = Delete fc
        compareFormattingForUnchangedChars (Patience.New (ComparePlainText fc)) = Insert fc
        compareFormattingForUnchangedChars (Patience.Both (ComparePlainText fc1) (ComparePlainText fc2)) = compareCharFormatting fc1 fc2

    compareCharFormatting :: FormattedCharacter -> FormattedCharacter -> RichTextDiffOp FormattedCharacter
    compareCharFormatting fc1 fc2 =
      if normalizedC1Marks == normalizedC2Marks
        then Copy fc1
        else UpdateMarks (MarkDiff normalizedC1Marks normalizedC2Marks) fc2
      where
        normalizedC1Marks = normalizeMarks (charMarks fc1)
        normalizedC2Marks = normalizeMarks (charMarks fc2)

        normalizeMarks :: [Mark] -> [Mark]
        normalizeMarks = sort

diffNoteRefTokens :: NoteRefToken -> NoteRefToken -> [RichTextDiffOp NoteRefAtom]
diffNoteRefTokens t1@(NoteRefToken id1) t2@(NoteRefToken id2) =
  if t1 == t2
    then [Copy (NoteRefAtom id1)]
    else [Delete (NoteRefAtom id1), Insert (NoteRefAtom id2)]

buildAnnotatedInlineNodeFromDiff :: [RichTextDiffOp InlineAtom] -> [EditScript]
buildAnnotatedInlineNodeFromDiff = (fmap InlineEditScript) . groupSameMarkAndDiffOpAtoms

groupSameMarkAndDiffOpAtoms :: [RichTextDiffOp InlineAtom] -> [RichTextDiffOp InlineSpan]
groupSameMarkAndDiffOpAtoms = foldr groupOrAppendAdjacent []
  where
    -- This is the folding function for grouping the adjacent inline atoms if they can be merged
    groupOrAppendAdjacent :: RichTextDiffOp InlineAtom -> [RichTextDiffOp InlineSpan] -> [RichTextDiffOp InlineSpan]
    groupOrAppendAdjacent atomWithDiffOp [] = [fmap atomToSpan atomWithDiffOp]
    groupOrAppendAdjacent (atomWithDiffOp) (firstSpanWithDiffOp : rest)
      -- If the current atom is a CharacterAtom and the first span is an InlineText with the same marks,
      -- merge them by prepending the character to the text span, preserving the diff operation.
      | CharacterAtom fc <- unpackDiffOpValue atomWithDiffOp,
        InlineText ts <- unpackDiffOpValue firstSpanWithDiffOp,
        diffOpSame atomWithDiffOp firstSpanWithDiffOp && charMarks fc == marks ts =
          (fmap (replaceWithMergedSpan (char fc) ts) firstSpanWithDiffOp) : rest
      | otherwise =
          fmap atomToSpan atomWithDiffOp : firstSpanWithDiffOp : rest

    -- Helper function that replaces any existing InlineSpan with a new
    -- InlineText constructed by prepending a Char to an existing TextSpan.
    replaceWithMergedSpan :: Char -> TextSpan -> InlineSpan -> InlineSpan
    replaceWithMergedSpan c ts _ = InlineText (mergeCharToTextSpan c ts)
      where
        mergeCharToTextSpan :: Char -> TextSpan -> TextSpan
        mergeCharToTextSpan ch textSpan = TextSpan (mergeCharToText ch (value textSpan)) (marks textSpan)

        mergeCharToText :: Char -> T.Text -> T.Text
        mergeCharToText ch txt = ch `T.cons` txt

    atomToSpan :: InlineAtom -> InlineSpan
    atomToSpan (CharacterAtom (FormattedCharacter c cMarks)) = InlineText (TextSpan (T.pack [c]) cMarks)
    atomToSpan (NoteAtom (NoteRefAtom noteId)) = NoteRef noteId

diffOpSame :: RichTextDiffOp a -> RichTextDiffOp b -> Bool
diffOpSame wrappedWithDiff1 wrappedWithDiff2 = getDiffOpType wrappedWithDiff1 == getDiffOpType wrappedWithDiff2

-- TODO: Use op type and make it parametric
replaceWithInsOp :: Edit a -> Edit a
replaceWithInsOp (Cpy node) = Ins node
replaceWithInsOp (Ins node) = Ins node
replaceWithInsOp (Del node) = Ins node
replaceWithInsOp (Swp _ node2) = Ins node2

replaceWithDelOp :: Edit a -> Edit a
replaceWithDelOp (Cpy node) = Del node
replaceWithDelOp (Ins node) = Del node
replaceWithDelOp (Del node) = Del node
replaceWithDelOp (Swp _ node2) = Del node2
