module Language.Doczen.Parser (
  parseDocument
) where

import Control.Applicative ((*>), (<*), (<$>), (<*>), pure)
import Text.Parsec

import Language.Doczen.Types

parseDocument :: String -> Either ParseError Document
parseDocument = parse (document <* eof) "doczen"

document = Document <$> header <*> sections

header = Header <$> ((lookAhead separator *> pure "") <|> line <|> (eof *> pure ""))

separator = (try $ symbol "---") *> pure ()

sections = (separator *> many section) <|> (eof *> pure [])

-- TODO add a no repl option
section = norepl <|> withrepl
  where
    norepl = ((try $ noreplSymbol <* eof) *> (pure $ Section False [])) <|> Section <$> norepl' <*> items'
    withrepl = Section True <$> items'
    norepl' = (try $ noreplSymbol *> pure False) <|> pure True
    noreplSymbol = symbol "{~ norepl ~}"

items' = emptySection <|> nonEmptySection
  where
    emptySection = separator *> pure []
    nonEmptySection = (:) <$> item <*> manyTill item (separator <|> eof)

item = try heading <|> try code <|> try runnableCode <|> paragraph

heading = Heading <$> hl <*> line
  where hl = (headingFromInt . length) <$> ((many1 $ char '#') <* (many $ oneOf " \t"))

code = Code <$> (string "```\n" *> manyTill anyChar (try $ string "```\n"))

runnableCode = RunnableCode <$> (string "```runnable\n" *> manyTill anyChar (try $ string "```\n"))

paragraph = Paragraph <$> line

line = (many1 (noneOf "\n")) <* (eof <|> newlines)

newlines = pure () <* (many1 $ char '\n')

symbol s = string s <* spaces

headingFromInt 1 = H1
headingFromInt 2 = H2
headingFromInt 3 = H3
headingFromInt 4 = H4
headingFromInt x = error ("heading level " ++ (show x) ++ " not supported.")
