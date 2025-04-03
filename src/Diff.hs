module Diff
  ( diff,
    getEditScript,
  )
where

import Data.Tree (Tree, unfoldTree)
import Data.TreeDiff (Edit (..))
import Data.TreeDiff.Tree (EditTree (..), treeDiff)
import DocTree (DocNode (..), TreeNode (..), toPandoc, toTree, traceTree)
import Text.Pandoc.Definition as Pandoc (Pandoc)

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
unfoldAnnotatedTreeFromEditScript :: Edit (EditTree DocNode) -> Tree DocNode
unfoldAnnotatedTreeFromEditScript = unfoldTree annotatedTreeNodeUnfolder

annotatedTreeNodeUnfolder :: Edit (EditTree DocNode) -> (DocNode, [Edit (EditTree DocNode)])
-- Leave Cpy nodes unchanged. Just return their sub-forest edit scripts as the next seeds to be unfolded.
annotatedTreeNodeUnfolder (Cpy (EditNode docNode subForestEditScripts)) = (docNode, subForestEditScripts)
annotatedTreeNodeUnfolder (Ins (EditNode (Root) subForestEditScripts)) = (Root, map replaceWithInsOp subForestEditScripts)
annotatedTreeNodeUnfolder (Del (EditNode (Root) subForestEditScripts)) = (Root, map replaceWithDelOp subForestEditScripts)
annotatedTreeNodeUnfolder (Swp (EditNode (Root) subForest1EditScripts) (EditNode (Root) subForest2EditScripts)) = undefined
annotatedTreeNodeUnfolder (Ins (EditNode (TreeNode (BlockNode blockNode)) subForestEditScripts)) = (TreeNode $ BlockNode blockNode, map replaceWithInsOp subForestEditScripts)
annotatedTreeNodeUnfolder (Del (EditNode (TreeNode (BlockNode blockNode)) subForestEditScripts)) = (TreeNode $ BlockNode blockNode, map replaceWithDelOp subForestEditScripts)
annotatedTreeNodeUnfolder (Swp (EditNode (TreeNode (BlockNode blockNode1)) subForest1EditScripts) (EditNode (TreeNode (BlockNode blockNode2)) subForest2EditScripts)) = undefined
annotatedTreeNodeUnfolder (Ins (EditNode (TreeNode (InlineNode inlineNode)) subForestEditScripts)) = undefined
annotatedTreeNodeUnfolder (Del (EditNode (TreeNode (InlineNode inlineNode)) subForestEditScripts)) = undefined
annotatedTreeNodeUnfolder (Swp (EditNode (TreeNode (InlineNode inlineNode1)) subForest1EditScripts) (EditNode (TreeNode (BlockNode inlineNode2)) subForest2EditScripts)) = undefined
-- TODO: Here we must return an error because we are in cases where the edit script is wrong (e.g. trying to replace the Root node with another block or inline node).
annotatedTreeNodeUnfolder _ = undefined

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