module Diff
  ( diff,
    getEditScript,
  )
where

import Data.Tree (Tree)
import Data.TreeDiff (Edit)
import Data.TreeDiff.Tree (EditTree, treeDiff)
import DocTree (DocNode, toPandoc, toTree, traceTree)
import Text.Pandoc.Definition as Pandoc (Pandoc)

diff :: Pandoc.Pandoc -> Pandoc.Pandoc -> Pandoc.Pandoc
diff pandoc1 pandoc2 = toPandoc $ annotateTreeWithDiffs tree1 tree2 editScript
  where
    tree1 = traceTree $ toTree pandoc1
    tree2 = traceTree $ toTree pandoc2
    -- Diff the 2 trees and get the edit script
    editScript = treeDiff tree1 tree2

-- TODO: Remove when all the components of the diff function (annotateTreeWithDiffs, toPandoc) are implemented
getEditScript :: Pandoc.Pandoc -> Pandoc.Pandoc -> Edit (EditTree DocNode)
getEditScript pandoc1 pandoc2 = treeDiff (traceTree $ toTree pandoc1) (traceTree $ toTree pandoc2)

-- Annotate/decorate the second tree by processing the edit script that contains the diffs
annotateTreeWithDiffs :: Tree DocNode -> Tree DocNode -> Edit (EditTree DocNode) -> Tree DocNode
annotateTreeWithDiffs = undefined