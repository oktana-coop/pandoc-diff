module Lib
  ( diff,
  )
where

import Data.TreeDiff (Edit)
import Data.TreeDiff.Tree (EditTree)
import Text.Pandoc.Definition (Pandoc)

diff :: Pandoc -> Pandoc -> Edit (EditTree a)
diff = undefined
