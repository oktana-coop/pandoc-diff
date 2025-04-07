module DocTree.LeafTextSpans (DocNode (..), TreeNode (..), InlineNode (..)) where

import DocTree.Common (BlockNode (..), TextSpan (..))
import Text.Pandoc.Definition as Pandoc (Inline (..))

data InlineNode = PandocInlines [Pandoc.Inline] deriving (Show, Eq)

data TreeNode = BlockNode BlockNode | InlineNode InlineNode | InlineContent TextSpan deriving (Show, Eq)

data DocNode = Root | TreeNode TreeNode deriving (Show, Eq)
