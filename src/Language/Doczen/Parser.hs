module Language.Doczen.Parser (
  documentParser
) where

import Control.Applicative ((*>), (<*), (<$>), (<*>), pure)
import Text.Parsec

import Language.Doczen.Types

documentParser :: Stream s m Char => ParsecT s u m Document
documentParser = document <* spaces <* eof

document :: Stream s m Char => ParsecT s u m Document
document = Document <$> header <*> sections

header :: Stream s m Char => ParsecT s u m Header
header = Header <$> line

separator :: Stream s m Char => ParsecT s u m ()
separator = (try $ symbol "---") *> pure ()

sections :: Stream s m Char => ParsecT s u m [Section]
sections = separator *> many section

section :: Stream s m Char => ParsecT s u m Section
-- TODO add a no repl option
section = Section True <$> items'

items' :: Stream s m Char => ParsecT s u m [Item]
items' = (:) <$> item <*> manyTill item (separator <|> eof)

item :: Stream s m Char => ParsecT s u m Item
item = try heading <|> try code <|> try runnableCode <|> paragraph

heading :: Stream s m Char => ParsecT s u m Item
heading = Heading <$> hl <*> line
  where hl = (headingFromInt . length) <$> (many1 $ symbol "#")

code :: Stream s m Char => ParsecT s u m Item
code = Code <$> (string "```\n" *> manyTill anyChar (try $ string "```\n"))

runnableCode :: Stream s m Char => ParsecT s u m Item
runnableCode = RunnableCode <$> (string "```runnable\n" *> manyTill anyChar (try $ string "```\n"))

paragraph :: Stream s m Char => ParsecT s u m Item
paragraph = Paragraph <$> line

line :: Stream s m Char => ParsecT s u m String
line = (many (noneOf "\n")) <* newlines

newlines :: Stream s m Char => ParsecT s u m String
newlines = many1 $ char '\n'

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol s = string s <* spaces