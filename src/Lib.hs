module Lib
  ( diff,
  )
where

import Data.TreeDiff (Edit)
import Data.TreeDiff.Tree (EditTree)
import Text.Pandoc.Definition (Block, Inline, Pandoc)

data BlockNode = PandocBlock Block | BulletListItem | OrderedListItem deriving (Show)

data InlineNode = PandocInline Inline deriving (Show)

data DocNode = Root | BlockNode BlockNode | InlineNode InlineNode deriving (Show)

diff :: Pandoc -> Pandoc -> Edit (EditTree DocNode)
diff = undefined
