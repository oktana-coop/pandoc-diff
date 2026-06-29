module TreeNotation.Utils (readFilesAndProduceTreeDiff) where

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Data.Tree (Tree, drawTree)
import RichTextDiff (getAnnotatedTree)
import Text.Pandoc (Pandoc, ReaderOptions, def, handleError, readMarkdown, readerExtensions)
import Text.Pandoc.Class (runIO)
import Text.Pandoc.Extensions (Extension (Ext_fenced_code_blocks, Ext_footnotes, Ext_smart, Ext_yaml_metadata_block), disableExtension, enableExtension, pandocExtensions)

-- Read two Markdown files and render the annotated diff tree — the diff.txt contents.
readFilesAndProduceTreeDiff :: FilePath -> FilePath -> IO BL.ByteString
readFilesAndProduceTreeDiff input1FilePath input2FilePath = do
  input1Text <- TIO.readFile input1FilePath
  input2Text <- TIO.readFile input2FilePath
  doc1 <- parseMarkdown input1Text
  doc2 <- parseMarkdown input2Text
  return $ BL.fromStrict (TE.encodeUtf8 (T.pack (renderDiffTree (getAnnotatedTree doc1 doc2))))

-- drawTree . fmap show — total by construction (no diff-op payload can be omitted).
renderDiffTree :: (Show a) => Tree a -> String
renderDiffTree = drawTree . fmap show

-- Normalization extensions (e.g. Ext_smart) should be disabled when diffing, so the
-- diff stays faithful to the actual content.
testReaderOptions :: ReaderOptions
testReaderOptions =
  def
    { readerExtensions =
        disableExtension Ext_smart $
          enableExtension Ext_footnotes $
            enableExtension Ext_fenced_code_blocks $
              enableExtension Ext_yaml_metadata_block pandocExtensions
    }

parseMarkdown :: T.Text -> IO Pandoc
parseMarkdown t = runIO (readMarkdown testReaderOptions t) >>= handleError
