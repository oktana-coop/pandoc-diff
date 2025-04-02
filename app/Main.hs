module Main (main) where

import Cli (Command (..), Format (..), readInputCommand)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Lib (diff)
import Text.Pandoc (Pandoc, PandocIO, ReaderOptions, def, handleError, readHtml, readJSON, readMarkdown, readNative)
import Text.Pandoc.Class (runIO)

readFrom :: Format -> ReaderOptions -> T.Text -> PandocIO Pandoc
readFrom format = case format of
  Cli.Pandoc -> readNative
  Cli.Markdown -> readMarkdown
  Cli.Html -> readHtml
  Cli.Json -> readJSON

main :: IO ()
main = do
  (Diff format doc1Str doc2Str) <- readInputCommand
  eitherDoc1 <- runIO $ readFrom format def (T.pack doc1Str)
  doc1 <- handleError eitherDoc1
  eitherDoc2 <- runIO $ readFrom format def (T.pack doc2Str)
  doc2 <- handleError eitherDoc2
  TIO.putStrLn $ T.pack $ show $ diff doc1 doc2
