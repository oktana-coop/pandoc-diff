module RichTextDiff
  ( diff,
    getEditScript,
  )
where

import Data.Algorithm.Diff as ListDiff (Diff, getDiff)
import qualified Data.Text as T
import Data.Tree (Tree, unfoldTree)
import Data.TreeDiff (Edit (..))
import Data.TreeDiff.Tree (EditTree (..), treeDiff)
import DocTree.Common (Mark (..), TextSpan (..))
import DocTree.GroupedInlines (BlockNode (..), DocNode (..), InlineNode (..), TreeNode (..), toTree, traceTree)
import DocTree.LeafTextSpans (DocNode (..), TreeNode (..))
import Text.Pandoc.Definition as Pandoc (Block (Div), Pandoc, nullAttr)

data FormattedCharacter = FormattedCharacter {char :: Char, charMarks :: [Mark]} deriving (Show, Eq)

data MarkDiff = MarkDiff [Mark] [Mark] deriving (Show, Eq)

data HeadingLevelDiff = HeadingLevelDiff Int Int deriving (Show, Eq)

data RichTextDiffOp a = Insert a | Delete a | Copy a | UpdateMarks MarkDiff a | UpdateHeadingLevel HeadingLevelDiff a deriving (Show, Eq)

data RichTextDiffOpType
  = InsertType
  | DeleteType
  | CopyType
  | UpdateMarksType
  | UpdateHeadingLevelType
  deriving (Show, Eq)

getDiffOpType :: RichTextDiffOp a -> RichTextDiffOpType
getDiffOpType (Insert _) = InsertType
getDiffOpType (Delete _) = DeleteType
getDiffOpType (Copy _) = CopyType
getDiffOpType (UpdateMarks _ _) = UpdateMarksType
getDiffOpType (UpdateHeadingLevel _ _) = UpdateHeadingLevelType

unpackDiffOpValue :: RichTextDiffOp a -> a
unpackDiffOpValue (Insert a) = a
unpackDiffOpValue (Delete a) = a
unpackDiffOpValue (Copy a) = a
unpackDiffOpValue (UpdateMarks _ a) = a
unpackDiffOpValue (UpdateHeadingLevel _ a) = a

instance Functor RichTextDiffOp where
  fmap f (Insert a) = Insert (f a)
  fmap f (Delete a) = Delete (f a)
  fmap f (Copy a) = Copy (f a)
  fmap f (UpdateMarks markDiff a) = UpdateMarks markDiff (f a)
  fmap f (UpdateHeadingLevel levelDiff a) = UpdateHeadingLevel levelDiff (f a)

data EditScript = TreeEditScript (Edit (EditTree DocTree.GroupedInlines.DocNode)) | InlineEditScript (RichTextDiffOp TextSpan) deriving (Show)

diff :: Pandoc.Pandoc -> Pandoc.Pandoc -> Pandoc.Pandoc
diff pandoc1 pandoc2 = (toPandoc . unfoldAnnotatedTreeFromEditScript) editScript
  where
    tree1 = traceTree $ DocTree.GroupedInlines.toTree pandoc1
    tree2 = traceTree $ DocTree.GroupedInlines.toTree pandoc2
    -- Diff the 2 trees and get the edit script
    editScript = TreeEditScript $ treeDiff tree1 tree2

-- TODO: Remove when all the components of the diff function (annotateTreeWithDiffs, toPandoc) are implemented
getEditScript :: Pandoc.Pandoc -> Pandoc.Pandoc -> EditScript
getEditScript pandoc1 pandoc2 = TreeEditScript $ treeDiff (traceTree $ DocTree.GroupedInlines.toTree pandoc1) (traceTree $ DocTree.GroupedInlines.toTree pandoc2)

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
  (Copy $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.BlockNode blockNode, map (TreeEditScript . replaceWithInsOp) subForestEditScripts)
annotatedTreeNodeUnfolder (TreeEditScript (Ins (EditNode (DocTree.GroupedInlines.TreeNode (DocTree.GroupedInlines.BlockNode blockNode)) subForestEditScripts))) =
  (Insert $ DocTree.LeafTextSpans.TreeNode $ DocTree.LeafTextSpans.BlockNode blockNode, map TreeEditScript subForestEditScripts)
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
-- Other cases (taking different types of nodes as input from the edit script)
-- TODO: Here we must return an error because we are in cases where the edit script is wrong (e.g. trying to replace the Root node with another block or inline node).
annotatedTreeNodeUnfolder _ = undefined

handleSwappedSubForests :: [Edit (EditTree DocTree.GroupedInlines.DocNode)] -> [Edit (EditTree DocTree.GroupedInlines.DocNode)] -> [EditScript]
handleSwappedSubForests deletedSubForests insertedSubForests = (map (TreeEditScript . replaceWithDelOp) deletedSubForests) <> (map (TreeEditScript . replaceWithInsOp) insertedSubForests)

diffInlineNodes :: DocTree.GroupedInlines.InlineNode -> DocTree.GroupedInlines.InlineNode -> [EditScript]
diffInlineNodes deletedInlineNode addedInlineNode = buildAnnotatedInlineNodeFromDiff $ ListDiff.getDiff (toFormattedText deletedInlineNode) (toFormattedText addedInlineNode)

toFormattedText :: DocTree.GroupedInlines.InlineNode -> [FormattedCharacter]
toFormattedText (DocTree.GroupedInlines.InlineContent textSpans) = concatMap textSpanToFormattedText textSpans

textSpanToFormattedText :: TextSpan -> [FormattedCharacter]
textSpanToFormattedText textSpan = map (\c -> FormattedCharacter c (marks textSpan)) $ T.unpack (value textSpan)

buildAnnotatedInlineNodeFromDiff :: [ListDiff.Diff FormattedCharacter] -> [EditScript]
buildAnnotatedInlineNodeFromDiff listDiff = fmap InlineEditScript $ groupSameMarkAndDiffOpChars $ map listDiffToRichTextDiff listDiff

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
        then (fmap (appendCharToTextSpan c) firstOfRest) : rest
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

    appendCharToTextSpan :: Char -> TextSpan -> TextSpan
    appendCharToTextSpan c textSpan = TextSpan (appendChar c (value textSpan)) (marks textSpan)

    appendChar :: Char -> T.Text -> T.Text
    appendChar c txt = txt `T.snoc` c

listDiffToRichTextDiff :: ListDiff.Diff FormattedCharacter -> RichTextDiffOp FormattedCharacter
listDiffToRichTextDiff = undefined

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
