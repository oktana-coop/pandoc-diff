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

data Mark = EmphMark | StrongMark | LinkMark LinkMark deriving (Show, Eq)

data TextSpan = TextSpan {value :: T.Text, marks :: [Mark]} deriving (Show, Eq)

instance Semigroup TextSpan where
  (<>) (TextSpan value1 marks1) (TextSpan value2 marks2) = TextSpan (value1 <> value2) (marks1 <> marks2)

instance Monoid TextSpan where
  mempty = TextSpan T.empty []

data InlineNode = InlineContent [TextSpan] deriving (Show, Eq)

data TreeNode = BlockNode BlockNode | InlineNode InlineNode deriving (Show, Eq)

data DocNode = Root | TreeNode TreeNode deriving (Show, Eq)

traceTree :: Tree DocNode -> Tree DocNode
traceTree tree = Debug.Trace.trace (drawTree $ fmap show tree) tree

toTree :: Pandoc.Pandoc -> Tree DocNode
toTree (Pandoc.Pandoc _ blocks) = Node Root $ unfoldForest treeNodeUnfolder $ map (BlockNode . PandocBlock) blocks

treeNodeUnfolder :: TreeNode -> (DocNode, [TreeNode])
treeNodeUnfolder (BlockNode blockNode) = blockTreeNodeUnfolder blockNode
treeNodeUnfolder (InlineNode inlineNode) = inlineTreeNodeUnfolder inlineNode

blockTreeNodeUnfolder :: BlockNode -> (DocNode, [TreeNode])
blockTreeNodeUnfolder (PandocBlock block) = case block of
  Pandoc.Plain inlines -> ((TreeNode . BlockNode . PandocBlock . Pandoc.Plain) [], [buildInlineNode inlines])
  Pandoc.Para inlines -> ((TreeNode . BlockNode . PandocBlock . Pandoc.Para) [], [buildInlineNode inlines])
  Pandoc.Header level attrs inlines -> (TreeNode . BlockNode $ PandocBlock $ Pandoc.Header level attrs [], [buildInlineNode inlines])
  Pandoc.CodeBlock attrs text -> (TreeNode . BlockNode $ PandocBlock $ Pandoc.CodeBlock attrs text, [])
  Pandoc.BulletList items -> ((TreeNode . BlockNode . PandocBlock . Pandoc.BulletList) [], map (BlockNode . ListItem) items)
  Pandoc.OrderedList attrs items -> (TreeNode $ BlockNode $ PandocBlock $ Pandoc.OrderedList attrs [], map (BlockNode . ListItem) items)
  _ -> undefined
blockTreeNodeUnfolder (ListItem children) = ((TreeNode . BlockNode . ListItem) [], map (BlockNode . PandocBlock) children)

buildInlineNode :: [Pandoc.Inline] -> TreeNode
buildInlineNode inlines = InlineNode $ InlineContent $ inlinesToTextSpans inlines

inlinesToTextSpans :: [Pandoc.Inline] -> [TextSpan]
inlinesToTextSpans = mergeSameMarkSpans . foldMap inlineToTextSpan

mergeSameMarkSpans :: [TextSpan] -> [TextSpan]
mergeSameMarkSpans = foldr mergeOrAppendAdjacent []
  where
    -- This is the folding function for merging the adjacent elements if their marks are the same
    mergeOrAppendAdjacent :: TextSpan -> [TextSpan] -> [TextSpan]
    mergeOrAppendAdjacent x [] = [x]
    -- pattern-match on: the current element (x), the one to its right (firstOfRest) and the rest of the fold
    mergeOrAppendAdjacent x (firstOfRest : rest) =
      if marks x == marks firstOfRest
        -- if the element's marks are the same with the one to its right, we merge them and then add them to the rest of the fold.
        then (x <> firstOfRest) : rest
        -- if they are not the same we end up with an extra text span in the list for the current element (we prepend it to the existing list for the fold.)
        else x : firstOfRest : rest

inlineToTextSpan :: Pandoc.Inline -> [TextSpan]
inlineToTextSpan inline = case inline of
  Pandoc.Str str -> [TextSpan str []]
  Pandoc.Space -> [TextSpan (T.pack " ") []]
  Pandoc.Strong inlines -> addMark StrongMark inlines
  Pandoc.Emph inlines -> addMark EmphMark inlines
  Pandoc.Link attrs inlines target -> addMark (LinkMark $ Lib.Link attrs target) inlines
  -- TODO: Handle other inline elements
  _ -> []

addMark :: Mark -> [Pandoc.Inline] -> [TextSpan]
-- Monoidally add the mark to all text spans created for the inline elements
addMark mark inlines = fmap (TextSpan T.empty [mark] <>) (inlinesToTextSpans inlines)

inlineTreeNodeUnfolder :: InlineNode -> (DocNode, [TreeNode])
inlineTreeNodeUnfolder inlineNode = (TreeNode $ InlineNode inlineNode, [])

diff :: Pandoc -> Pandoc -> Edit (EditTree DocNode)
diff pandoc1 pandoc2 = treeDiff (traceTree $ toTree pandoc1) (traceTree $ toTree pandoc2)
