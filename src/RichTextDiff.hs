{-# LANGUAGE InstanceSigs #-}

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
import DocTree.Common (Mark (..), TextSpan (..))
import DocTree.GroupedInlines (BlockNode (..), DocNode (..), InlineNode (..), TreeNode (..), toTree)
import DocTree.LeafTextSpans (DocNode (..), TreeNode (..))
import Patience (Item (..), diff)
import RichTextAnalysis (FormattedCharacter (..), FormattedToken (..), textSpanToFormattedText, tokenizeFormattedText)
import RichTextDiffOp (MarkDiff (..), RichTextDiffOp (..), getDiffOpType, unpackDiffOpValue)
import Text.Pandoc.Definition as Pandoc (Block (Div), Pandoc, nullAttr)

-- Helper wrapper type used to compare the plain text (ignore formatting) when using the (patience) diff algorithm for characters.
newtype ComparePlainText = ComparePlainText FormattedCharacter

instance Eq ComparePlainText where
  (==) :: ComparePlainText -> ComparePlainText -> Bool
  (ComparePlainText (FormattedCharacter c1 _)) == (ComparePlainText (FormattedCharacter c2 _)) = c1 == c2

instance Ord ComparePlainText where
  compare :: ComparePlainText -> ComparePlainText -> Ordering
  compare (ComparePlainText a) (ComparePlainText b) = compare (char a) (char b)

newtype CompareTokenText = CompareTokenText FormattedToken

-- Helper wrapper type used to compare the plain text (ignore formatting) when using the (patience) diff algorithm for words/tokens.
instance Eq CompareTokenText where
  (==) :: CompareTokenText -> CompareTokenText -> Bool
  (CompareTokenText t1) == (CompareTokenText t2) = tokenText t1 == tokenText t2

instance Ord CompareTokenText where
  compare :: CompareTokenText -> CompareTokenText -> Ordering
  compare (CompareTokenText t1) (CompareTokenText t2) = compare (tokenText t1) (tokenText t2)

data EditScript = TreeEditScript (Edit (EditTree DocTree.GroupedInlines.DocNode)) | InlineEditScript (RichTextDiffOp TextSpan) deriving (Show)

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
-- In this case of swapping blocks, we add a wrapper div container to the tree and create del+ins operations for the swapped blocks respectively.
annotatedTreeNodeUnfolder (TreeEditScript (Swp block1@(EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode _)) _) block2@(EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode _)) _))) =
  (Copy $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.BlockNode $ PandocBlock $ Pandoc.Div nullAttr [], [(TreeEditScript . Del) block1, (TreeEditScript . Ins) block2])
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

handleSwappedSubForests :: [Edit (EditTree DocTree.GroupedInlines.DocNode)] -> [Edit (EditTree DocTree.GroupedInlines.DocNode)] -> [EditScript]
handleSwappedSubForests deletedSubForests insertedSubForests = (map (TreeEditScript . replaceWithDelOp) deletedSubForests) <> (map (TreeEditScript . replaceWithInsOp) insertedSubForests)

diffInlineNodes :: DocTree.GroupedInlines.InlineNode -> DocTree.GroupedInlines.InlineNode -> [EditScript]
diffInlineNodes deletedInlineNode addedInlineNode = buildAnnotatedInlineNodeFromDiff $ diffFormattedTokens ((tokenizeFormattedText . toFormattedText) deletedInlineNode) ((tokenizeFormattedText . toFormattedText) addedInlineNode)

toFormattedText :: DocTree.GroupedInlines.InlineNode -> [FormattedCharacter]
toFormattedText (DocTree.GroupedInlines.InlineContent textSpans) = concatMap textSpanToFormattedText textSpans

diffFormattedTokens :: [FormattedToken] -> [FormattedToken] -> [RichTextDiffOp FormattedCharacter]
diffFormattedTokens tokens1 tokens2 = concatMap resolveTokenDiff $ Patience.diff (map CompareTokenText tokens1) (map CompareTokenText tokens2)
  where
    resolveTokenDiff :: Patience.Item CompareTokenText -> [RichTextDiffOp FormattedCharacter]
    resolveTokenDiff (Patience.Old (CompareTokenText t)) = map Delete (tokenChars t)
    resolveTokenDiff (Patience.New (CompareTokenText t)) = map Insert (tokenChars t)
    resolveTokenDiff (Patience.Both (CompareTokenText t1) (CompareTokenText t2)) =
      if tokenText t1 == tokenText t2
        then diffFormatting t1 t2
        -- Fallback char-by-char diff
        else diffFormattedText (tokenChars t1) (tokenChars t2)

    diffFormatting :: FormattedToken -> FormattedToken -> [RichTextDiffOp FormattedCharacter]
    diffFormatting (FormattedToken _ chars1) (FormattedToken _ chars2) = zipWith compareCharFormatting chars1 chars2

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

buildAnnotatedInlineNodeFromDiff :: [RichTextDiffOp FormattedCharacter] -> [EditScript]
buildAnnotatedInlineNodeFromDiff = (fmap InlineEditScript) . groupSameMarkAndDiffOpChars

groupSameMarkAndDiffOpChars :: [RichTextDiffOp FormattedCharacter] -> [RichTextDiffOp TextSpan]
groupSameMarkAndDiffOpChars = foldr groupOrAppendAdjacent []
  where
    -- This is the folding function for grouping the adjacent characters if their marks are the same
    groupOrAppendAdjacent :: RichTextDiffOp FormattedCharacter -> [RichTextDiffOp TextSpan] -> [RichTextDiffOp TextSpan]
    -- Keep the diff op the same (just fmap over it) and create a text span with just this character
    groupOrAppendAdjacent formattedCharWithDiffOp [] = [fmap textSpanFromFormattedChar formattedCharWithDiffOp]
    -- pattern-match on: the current element (x), the one to its right (firstOfRest) and the rest of the fold
    groupOrAppendAdjacent formattedCharWithDiffOp (firstOfRest : rest) =
      if (diffOpSame formattedCharWithDiffOp firstOfRest && characterAndTextSpanMarksSame formattedCharWithDiffOp firstOfRest)
        -- if the element's marks are the same with the one to its right, we merge them and then add them to the rest of the fold.
        then (fmap (mergeCharToTextSpan c) firstOfRest) : rest
        -- if they are not the same we end up with an extra text span in the list for the current element (we prepend it to the existing list for the fold.)
        else fmap textSpanFromFormattedChar formattedCharWithDiffOp : firstOfRest : rest
      where
        c = (char . unpackDiffOpValue) formattedCharWithDiffOp

    textSpanFromFormattedChar :: FormattedCharacter -> TextSpan
    textSpanFromFormattedChar (FormattedCharacter c cMarks) = TextSpan (T.pack [c]) cMarks

    diffOpSame :: RichTextDiffOp a -> RichTextDiffOp b -> Bool
    diffOpSame wrappedWithDiff1 wrappedWithDiff2 = getDiffOpType wrappedWithDiff1 == getDiffOpType wrappedWithDiff2

    characterAndTextSpanMarksSame :: RichTextDiffOp FormattedCharacter -> RichTextDiffOp TextSpan -> Bool
    characterAndTextSpanMarksSame formattedCharWithDiffOp textSpan = (charMarks . unpackDiffOpValue) formattedCharWithDiffOp == (marks $ unpackDiffOpValue textSpan)

    mergeCharToTextSpan :: Char -> TextSpan -> TextSpan
    mergeCharToTextSpan c textSpan = TextSpan (mergeCharToText c (value textSpan)) (marks textSpan)

    mergeCharToText :: Char -> T.Text -> T.Text
    mergeCharToText c txt = c `T.cons` txt

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
