module Cli (Command (..), readInputCommand, Format (..)) where

import Options.Applicative (Parser, ReadM, argument, command, eitherReader, execParser, fullDesc, help, helper, info, long, metavar, option, progDesc, str, subparser, (<**>))

data Command
  = Diff Format String String
  deriving (Show)

data Format = Pandoc | Markdown | Html deriving (Show)

formatParser :: Parser Format
formatParser = option readFormat (long "from" <> metavar "FORMAT" <> help "Specify the format (pandoc, markdown, html)")

readFormat :: ReadM Format
readFormat = eitherReader $ \arg ->
  case arg of
    "pandoc" -> Right Pandoc
    "markdown" -> Right Markdown
    "html" -> Right Html
    _ -> Left $ "Unknown format: " ++ arg

commandParser :: Parser Command
commandParser = subparser (command "diff" (info (Diff <$> formatParser <*> argument str (metavar "DOC_1") <*> argument str (metavar "DOC_2")) (progDesc "Diff two pandoc documents")))

readInputCommand :: IO Command
readInputCommand = execParser (info (commandParser <**> helper) fullDesc)