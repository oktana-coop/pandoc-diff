module Cli (Command (..), readInputCommand, Format (..)) where

import Options.Applicative (Parser, ReadM, argument, eitherReader, execParser, fullDesc, help, helper, info, long, metavar, option, str, (<**>))

data Command
  = Diff Format String String
  deriving (Show)

data Format = Pandoc | Markdown | Html | Json deriving (Show)

formatParser :: Parser Format
formatParser = option readFormat (long "format" <> metavar "FORMAT" <> help "Specify the format (pandoc, markdown, html)")

readFormat :: ReadM Format
readFormat = eitherReader $ \arg ->
  case arg of
    "pandoc" -> Right Pandoc
    "markdown" -> Right Markdown
    "html" -> Right Html
    "json" -> Right Json
    _ -> Left $ "Unknown format: " ++ arg

commandParser :: Parser Command
commandParser = Diff <$> formatParser <*> argument str (metavar "DOC_1") <*> argument str (metavar "DOC_2")

readInputCommand :: IO Command
readInputCommand = execParser (info (commandParser <**> helper) fullDesc)