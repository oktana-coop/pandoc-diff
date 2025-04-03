module Diff
  ( diff,
  )
where

import Data.TreeDiff (Edit)
import Data.TreeDiff.Tree (EditTree, treeDiff)
import DocTree (DocNode, toTree, traceTree)
import Text.Pandoc.Definition as Pandoc (Pandoc)

diff :: Pandoc.Pandoc -> Pandoc.Pandoc -> Edit (EditTree DocNode)
diff pandoc1 pandoc2 = treeDiff (traceTree $ toTree pandoc1) (traceTree $ toTree pandoc2)
