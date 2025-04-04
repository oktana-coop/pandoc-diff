module Diff
  ( diff,
    getEditScript,
  )
where

import Data.Tree (Tree, unfoldTree)
import Data.TreeDiff (Edit (..))
import Data.TreeDiff.Tree (EditTree (..), treeDiff)
import DocTree (BlockNode (..), DocNode (..), InlineNode (..), Mark (..), TextSpan (..), TreeNode (..), toTree, traceTree)
import Text.Pandoc.Definition as Pandoc (Block (Div), Pandoc, nullAttr)

data MarkDiff = MarkDiff [Mark] [Mark]

data HeadingLevelDiff = HeadingLevelDiff Int Int

data RichTextDiffOp a = Insert a | Delete a | Copy a | UpdateMarks MarkDiff a | UpdateHeadingLevel HeadingLevelDiff a

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
  (handleSwappedInlines inlineNode1 inlineNode2, handleSwappedSubForests subForest1EditScripts subForest2EditScripts)
-- TODO: Here we must return an error because we are in cases where the edit script is wrong (e.g. trying to replace the Root node with another block or inline node).
annotatedTreeNodeUnfolder _ = undefined

handleSwappedSubForests :: [Edit (EditTree DocNode)] -> [Edit (EditTree DocNode)] -> [Edit (EditTree DocNode)]
handleSwappedSubForests deletedSubForests insertedSubForests = (map replaceWithDelOp deletedSubForests) <> (map replaceWithInsOp insertedSubForests)

handleSwappedInlines :: InlineNode -> InlineNode -> RichTextDiffOp DocNode
handleSwappedInlines deletedInlineNode addedInlineNode = undefined

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
