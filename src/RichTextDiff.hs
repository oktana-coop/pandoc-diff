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
import DocTree.GroupedInlines (BlockNode (..), DocNode (..), InlineNode (..), Mark (..), TextSpan (..), TreeNode (..), toTree, traceTree)
import Text.Pandoc.Definition as Pandoc (Block (Div), Pandoc, nullAttr)

data FormattedCharacter = FormattedCharacter {char :: Char, charMarks :: [Mark]} deriving (Show, Eq)

data MarkDiff = MarkDiff [Mark] [Mark]

data HeadingLevelDiff = HeadingLevelDiff Int Int

data RichTextDiffOp a = Insert a | Delete a | Copy a | UpdateMarks MarkDiff a | UpdateHeadingLevel HeadingLevelDiff a

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

diff :: Pandoc.Pandoc -> Pandoc.Pandoc -> Pandoc.Pandoc
diff pandoc1 pandoc2 = (toPandoc . unfoldAnnotatedTreeFromEditScript) editScript
  where
    tree1 = traceTree $ toTree pandoc1
    tree2 = traceTree $ toTree pandoc2
    -- Diff the 2 trees and get the edit script
    editScript = treeDiff tree1 tree2

-- TODO: Remove when all the components of the diff function (annotateTreeWithDiffs, toPandoc) are implemented
getEditScript :: Pandoc.Pandoc -> Pandoc.Pandoc -> Edit (EditTree DocNode)
getEditScript pandoc1 pandoc2 = treeDiff (traceTree $ toTree pandoc1) (traceTree $ toTree pandoc2)

-- Produce the annotated tree from the edit script that contains the diffs
unfoldAnnotatedTreeFromEditScript :: Edit (EditTree DocNode) -> Tree (RichTextDiffOp DocNode)
unfoldAnnotatedTreeFromEditScript = unfoldTree annotatedTreeNodeUnfolder

toPandoc :: Tree (RichTextDiffOp DocNode) -> Pandoc.Pandoc
toPandoc = undefined

annotatedTreeNodeUnfolder :: Edit (EditTree DocNode) -> (RichTextDiffOp DocNode, [Edit (EditTree DocNode)])
-- Leave Cpy nodes unchanged. Just return their sub-forest edit scripts as the next seeds to be unfolded.
annotatedTreeNodeUnfolder (Cpy (EditNode docNode subForestEditScripts)) = (Copy docNode, subForestEditScripts)
annotatedTreeNodeUnfolder (Ins (EditNode (Root) subForestEditScripts)) = (Insert Root, map replaceWithInsOp subForestEditScripts)
annotatedTreeNodeUnfolder (Del (EditNode (Root) subForestEditScripts)) = (Delete Root, map replaceWithDelOp subForestEditScripts)
annotatedTreeNodeUnfolder (Swp (EditNode (Root) subForest1EditScripts) (EditNode (Root) subForest2EditScripts)) = (Copy Root, handleSwappedSubForests subForest1EditScripts subForest2EditScripts)
annotatedTreeNodeUnfolder (Ins (EditNode (TreeNode (BlockNode blockNode)) subForestEditScripts)) = (Insert $ TreeNode $ BlockNode blockNode, map replaceWithInsOp subForestEditScripts)
annotatedTreeNodeUnfolder (Del (EditNode (TreeNode (BlockNode blockNode)) subForestEditScripts)) = (Delete $ TreeNode $ BlockNode blockNode, map replaceWithDelOp subForestEditScripts)
-- In this case of swapping blocks, we add a wrapper div container to the tree and create del+ins operations for the swapped blocks respectively.
annotatedTreeNodeUnfolder (Swp block1@(EditNode (TreeNode (BlockNode _)) _) block2@(EditNode (TreeNode (BlockNode _)) _)) = (Copy $ TreeNode $ BlockNode $ PandocBlock $ Pandoc.Div nullAttr [], [Del block1, Ins block2])
annotatedTreeNodeUnfolder (Ins (EditNode (TreeNode (InlineNode inlineNode)) subForestEditScripts)) = (Insert $ TreeNode $ InlineNode inlineNode, map replaceWithInsOp subForestEditScripts)
annotatedTreeNodeUnfolder (Del (EditNode (TreeNode (InlineNode inlineNode)) subForestEditScripts)) = (Delete $ TreeNode $ InlineNode inlineNode, map replaceWithDelOp subForestEditScripts)
-- In the case of swapping inlines, we call `handleSwappedInlines` to do the heavy lifting for handling the nodes themselves and create the del+ins operations for the sub-forests
annotatedTreeNodeUnfolder (Swp (EditNode (TreeNode (InlineNode inlineNode1)) subForest1EditScripts) (EditNode (TreeNode (InlineNode inlineNode2)) subForest2EditScripts)) =
  (diffInlineNodes inlineNode1 inlineNode2, handleSwappedSubForests subForest1EditScripts subForest2EditScripts)
-- TODO: Here we must return an error because we are in cases where the edit script is wrong (e.g. trying to replace the Root node with another block or inline node).
annotatedTreeNodeUnfolder _ = undefined

handleSwappedSubForests :: [Edit (EditTree DocNode)] -> [Edit (EditTree DocNode)] -> [Edit (EditTree DocNode)]
handleSwappedSubForests deletedSubForests insertedSubForests = (map replaceWithDelOp deletedSubForests) <> (map replaceWithInsOp insertedSubForests)

diffInlineNodes :: InlineNode -> InlineNode -> RichTextDiffOp DocNode
diffInlineNodes deletedInlineNode addedInlineNode = buildAnnotatedInlineNodeFromDiff $ ListDiff.getDiff (toFormattedText deletedInlineNode) (toFormattedText addedInlineNode)

toFormattedText :: InlineNode -> [FormattedCharacter]
toFormattedText (InlineContent textSpans) = concatMap textSpanToFormattedText textSpans

textSpanToFormattedText :: TextSpan -> [FormattedCharacter]
textSpanToFormattedText textSpan = map (\c -> FormattedCharacter c (marks textSpan)) $ T.unpack (value textSpan)

buildAnnotatedInlineNodeFromDiff :: [ListDiff.Diff FormattedCharacter] -> RichTextDiffOp DocNode
buildAnnotatedInlineNodeFromDiff listDiff = Copy $ TreeNode $ InlineNode $ InlineContent $ groupSameMarkAndDiffOpChars $ map listDiffToRichTextDiff listDiff

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
