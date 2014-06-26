module Language.Doczen.Parser (
  parseDocument
) where

import Control.Applicative ((*>), (<*), (<$>), (<*>), pure)
import Text.Parsec

import Language.Doczen.Types

parseDocument :: String -> Either ParseError Document
parseDocument = parse (document <* eof) "doczen"

document :: Stream s m Char => ParsecT s u m Document
document = Document <$> header <*> sections

header :: Stream s m Char => ParsecT s u m Header
header = Header <$> ((lookAhead separator *> pure "") <|> line <|> (eof *> pure ""))

separator :: Stream s m Char => ParsecT s u m ()
separator = (try $ symbol "---") *> pure ()

sections :: Stream s m Char => ParsecT s u m [Section]
sections = (separator *> many section) <|> (eof *> pure [])

section :: Stream s m Char => ParsecT s u m Section
-- TODO add a no repl option
section = norepl <|> withrepl
  where
    norepl = ((try $ noreplSymbol <* eof) *> (pure $ Section False [])) <|> Section <$> norepl' <*> items'
    withrepl = Section True <$> items'
    norepl' = (try $ noreplSymbol *> pure False) <|> pure True
    noreplSymbol = symbol "{~ norepl ~}"

items' :: Stream s m Char => ParsecT s u m [Item]
items' = emptySection <|> nonEmptySection
  where
    emptySection = separator *> pure []
    nonEmptySection = (:) <$> item <*> manyTill item (separator <|> eof)

item :: Stream s m Char => ParsecT s u m Item
item = try heading <|> try code <|> try runnableCode <|> paragraph

heading :: Stream s m Char => ParsecT s u m Item
heading = Heading <$> hl <*> line
  where hl = (headingFromInt . length) <$> ((many1 $ char '#') <* (many $ oneOf " \t"))

code :: Stream s m Char => ParsecT s u m Item
code = Code <$> (string "```\n" *> manyTill anyChar (try $ string "```\n"))

runnableCode :: Stream s m Char => ParsecT s u m Item
runnableCode = RunnableCode <$> (string "```runnable\n" *> manyTill anyChar (try $ string "```\n"))

paragraph :: Stream s m Char => ParsecT s u m Item
paragraph = Paragraph <$> line

line :: Stream s m Char => ParsecT s u m String
line = (many1 (noneOf "\n")) <* (eof <|> newlines)

newlines :: Stream s m Char => ParsecT s u m ()
newlines = pure () <* (many1 $ char '\n')

symbol :: Stream s m Char => String -> ParsecT s u m String
symbol s = string s <* spaces

headingFromInt :: Int -> HeadingLevel
headingFromInt 1 = H1
headingFromInt 2 = H2
headingFromInt 3 = H3
headingFromInt 4 = H4
headingFromInt x = error ("heading level " ++ (show x) ++ " not supported.")
