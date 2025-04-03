module Lib
  ( diff,
  )
where

import qualified Data.Text as T
import Data.Tree (Tree (Node), drawTree, foldTree, unfoldForest)
import Data.TreeDiff (Edit)
import Data.TreeDiff.Tree (EditTree, treeDiff)
import Debug.Trace
import Text.Pandoc.Definition as Pandoc (Attr, Block (..), Inline (..), Pandoc (..), Target)

data BlockNode = PandocBlock Pandoc.Block | ListItem [Pandoc.Block] deriving (Show, Eq)

data LinkMark = Link Pandoc.Attr Pandoc.Target deriving (Show, Eq)

data Mark = Emph | Strong | LinkMark LinkMark deriving (Show, Eq)

data TextSpan = TextSpan T.Text [Mark] deriving (Show, Eq)

data InlineNode = InlineContent [TextSpan] deriving (Show, Eq)

data TreeNode = BlockNode BlockNode | InlineNode InlineNode deriving (Show, Eq)

data DocNode = Root | TreeNode TreeNode deriving (Show, Eq)

traceTree :: Tree DocNode -> Tree DocNode
traceTree tree = Debug.Trace.trace (drawTree $ fmap show tree) tree

toTree :: Pandoc -> Tree DocNode
toTree (Pandoc _ blocks) = Node Root $ unfoldForest treeNodeUnfolder $ map (BlockNode . PandocBlock) blocks

treeNodeUnfolder :: TreeNode -> (DocNode, [TreeNode])
treeNodeUnfolder (BlockNode blockNode) = blockTreeNodeUnfolder blockNode
treeNodeUnfolder (InlineNode inlineNode) = inlineTreeNodeUnfolder inlineNode

blockTreeNodeUnfolder :: BlockNode -> (DocNode, [TreeNode])
blockTreeNodeUnfolder (PandocBlock block) = case block of
  Plain inlines -> ((TreeNode . BlockNode . PandocBlock . Pandoc.Plain) [], [buildInlineNode inlines])
  Para inlines -> ((TreeNode . BlockNode . PandocBlock . Pandoc.Para) [], [buildInlineNode inlines])
  Header level attrs inlines -> (TreeNode . BlockNode $ PandocBlock $ Pandoc.Header level attrs [], [buildInlineNode inlines])
  CodeBlock attrs text -> (TreeNode . BlockNode $ PandocBlock $ Pandoc.CodeBlock attrs text, [])
  BulletList items -> ((TreeNode . BlockNode . PandocBlock . Pandoc.BulletList) [], map (BlockNode . ListItem) items)
  OrderedList attrs items -> (TreeNode $ BlockNode $ PandocBlock $ Pandoc.OrderedList attrs [], map (BlockNode . ListItem) items)
  _ -> undefined
blockTreeNodeUnfolder (ListItem children) = ((TreeNode . BlockNode . ListItem) [], map (BlockNode . PandocBlock) children)

buildInlineNode :: [Inline] -> TreeNode
buildInlineNode = undefined

inlineTreeNodeUnfolder :: InlineNode -> (DocNode, [TreeNode])
inlineTreeNodeUnfolder inlineNode = (TreeNode $ InlineNode inlineNode, [])

-- inlineTreeNodeUnfolder :: InlineNode -> (DocNode, [TreeNode])
-- inlineTreeNodeUnfolder (PandocInline inline) = case inline of
--   Str txt -> ((TreeNode . InlineNode . PandocInline . Str) txt, [])
--   Emph inlines -> ((TreeNode . InlineNode . PandocInline . Emph) [], map (InlineNode . PandocInline) inlines)
--   Underline inlines -> ((TreeNode . InlineNode . PandocInline . Underline) [], map (InlineNode . PandocInline) inlines)
--   Strong inlines -> ((TreeNode . InlineNode . PandocInline . Strong) [], map (InlineNode . PandocInline) inlines)
--   Strikeout inlines -> ((TreeNode . InlineNode . PandocInline . Strikeout) [], map (InlineNode . PandocInline) inlines)
--   Superscript inlines -> ((TreeNode . InlineNode . PandocInline . Superscript) [], map (InlineNode . PandocInline) inlines)
--   Subscript inlines -> ((TreeNode . InlineNode . PandocInline . Subscript) [], map (InlineNode . PandocInline) inlines)
--   SmallCaps inlines -> ((TreeNode . InlineNode . PandocInline . SmallCaps) [], map (InlineNode . PandocInline) inlines)
--   Space -> ((TreeNode . InlineNode . PandocInline) Space, [])
--   Link attrs inlines target -> (TreeNode $ InlineNode $ PandocInline $ Link attrs [] target, map (InlineNode . PandocInline) inlines)
--   Span attrs inlines -> (TreeNode $ InlineNode $ PandocInline $ Span attrs [], map (InlineNode . PandocInline) inlines)
--   _ -> undefined

diff :: Pandoc -> Pandoc -> Edit (EditTree DocNode)
diff pandoc1 pandoc2 = treeDiff (traceTree $ toTree pandoc1) (traceTree $ toTree pandoc2)
