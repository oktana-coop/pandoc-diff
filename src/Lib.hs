module Lib
  ( diff,
  )
where

import Data.Tree (Tree (Node), drawTree, foldTree, unfoldForest)
import Data.TreeDiff (Edit)
import Data.TreeDiff.Tree (EditTree, treeDiff)
import Debug.Trace
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..))

-- data PandocSeed = PandocBlockSeed Block | PandocInlineSeed Inline

data BlockNode = PandocBlock Block | ListItem [Block] deriving (Show, Eq)

data InlineNode = PandocInline Inline deriving (Show, Eq)

data TreeNode = BlockNode BlockNode | InlineNode InlineNode deriving (Show, Eq)

data DocNode = Root | TreeNode TreeNode deriving (Show, Eq)

traceTree :: Tree DocNode -> Tree DocNode
traceTree tree = Debug.Trace.trace (drawTree $ fmap show tree) tree

toTree :: Pandoc -> Tree DocNode
toTree (Pandoc _ blocks) = Node Root $ unfoldForest treeNodeUnfolder $ map (BlockNode . PandocBlock) blocks

treeNodeUnfolder :: TreeNode -> (DocNode, [TreeNode])
treeNodeUnfolder (BlockNode blockNode) = blockTreeNodeUnfolder blockNode
treeNodeUnfolder (InlineNode inline) = inlineTreeNodeUnfolder inline

blockTreeNodeUnfolder :: BlockNode -> (DocNode, [TreeNode])
blockTreeNodeUnfolder (PandocBlock block) = case block of
  Plain inlines -> ((TreeNode . BlockNode . PandocBlock . Plain) [], map (InlineNode . PandocInline) inlines)
  Para inlines -> ((TreeNode . BlockNode . PandocBlock . Para) [], map (InlineNode . PandocInline) inlines)
  Header level attrs inlines -> (TreeNode . BlockNode $ PandocBlock $ Header level attrs [], map (InlineNode . PandocInline) inlines)
  CodeBlock attrs text -> (TreeNode . BlockNode $ PandocBlock $ CodeBlock attrs text, [])
  BulletList items -> ((TreeNode . BlockNode . PandocBlock . BulletList) [], map (BlockNode . ListItem) items)
  OrderedList attrs items -> (TreeNode $ BlockNode $ PandocBlock $ OrderedList attrs [], map (BlockNode . ListItem) items)
  _ -> undefined
blockTreeNodeUnfolder (ListItem children) = ((TreeNode . BlockNode . ListItem) [], map (BlockNode . PandocBlock) children)

inlineTreeNodeUnfolder :: InlineNode -> (DocNode, [TreeNode])
inlineTreeNodeUnfolder (PandocInline inline) = case inline of
  Str txt -> ((TreeNode . InlineNode . PandocInline . Str) txt, [])
  Emph inlines -> ((TreeNode . InlineNode . PandocInline . Emph) [], map (InlineNode . PandocInline) inlines)
  Underline inlines -> ((TreeNode . InlineNode . PandocInline . Underline) [], map (InlineNode . PandocInline) inlines)
  Strong inlines -> ((TreeNode . InlineNode . PandocInline . Strong) [], map (InlineNode . PandocInline) inlines)
  Strikeout inlines -> ((TreeNode . InlineNode . PandocInline . Strikeout) [], map (InlineNode . PandocInline) inlines)
  Superscript inlines -> ((TreeNode . InlineNode . PandocInline . Superscript) [], map (InlineNode . PandocInline) inlines)
  Subscript inlines -> ((TreeNode . InlineNode . PandocInline . Subscript) [], map (InlineNode . PandocInline) inlines)
  SmallCaps inlines -> ((TreeNode . InlineNode . PandocInline . SmallCaps) [], map (InlineNode . PandocInline) inlines)
  Space -> ((TreeNode . InlineNode . PandocInline) Space, [])
  Link attrs inlines target -> (TreeNode $ InlineNode $ PandocInline $ Link attrs [] target, map (InlineNode . PandocInline) inlines)
  Span attrs inlines -> (TreeNode $ InlineNode $ PandocInline $ Span attrs [], map (InlineNode . PandocInline) inlines)
  _ -> undefined

diff :: Pandoc -> Pandoc -> Edit (EditTree DocNode)
diff pandoc1 pandoc2 = treeDiff (traceTree $ toTree pandoc1) (traceTree $ toTree pandoc2)
