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

separator = (try $ string "---") *> pure ()

sections = (separator *> many section) <|> (eof *> pure [])

section = newlines *> (try emptySection <|> fullSection)
  where
    emptySection = Section <$> (options <* eof) <*> pure []
    fullSection = Section <$> options <*> items'
    options = option' `sepEndBy` newlines
    option' = try norepl <|> secid
    norepl = string "!norepl" *> pure NoAttachedRepl
    secid = SectionId <$> (string "!id: " *> idfield)

idfield = many $ alphaNum <|> oneOf "-"

items' = emptySection <|> nonEmptySection
  where
    emptySection = separator *> pure []
    nonEmptySection = (:) <$> item <*> manyTill item (separator <|> eof)

item = try heading <|> try code <|> paragraph

heading = Heading <$> hl <*> enhancedText
  where hl = (headingFromInt . length) <$> ((many1 $ char '#') <* (many $ oneOf " \t"))

paragraph = Paragraph <$> enhancedText

code = Code <$> (string "```" *> codeOptions) <*> codeText
codeOptions = codeOption `endsWith` newline
codeOption = try runnable <|> try hidden <|> codeid
runnable = char '>' *> pure Runnable
hidden = char '~' *> pure Hidden
codeid = CodeId <$> (string " (" *> idfield <* char ')')

codeText = codeTextNode `endsWith` (string "```" <* eol)
codeTextNode = try blanks <|> regularCodeChar
blanks = Blanks <$> (largeBlanks <|> normalBlanks)
largeBlanks = string "______" *> pure LargeBlank
normalBlanks = string "____" *> pure NormalBlank
regularCodeChar = RCC <$> anyChar

enhancedText = enhancedTextNode `endsWith` eol
enhancedTextNode = try small
               <|> try em
               <|> try strong
               <|> try inlineCode
               <|> try tt
               <|> try smile
               <|> try smileP
               <|> try link
               <|> try html
               <|> regularChar
small = Small <$> wrapping enhancedTextNode (string "::")
em = Em <$> wrapping enhancedTextNode (string "__")
strong = Strong <$> wrapping enhancedTextNode (string "**")
inlineCode = InlineCode <$> wrapping (noneOf "`") (string "`")
tt = Tt <$> wrapping enhancedTextNode (string "++")
smile = ifString ":)" $ pure Smile
smileP = ifString ":P" $ pure SmileP
link = Link
  <$> (char '[' *> (enhancedTextNode `endsWith` char ']'))
  <*> (char '(' *> ((noneOf ")")     `endsWith` char ')'))
html = do
  _ <- char '<'
  tag <- many1 alphaNum
  let close = "</" ++ tag ++ ">"
  mid <- anyChar `endsWith` string close
  return $ Html $ "<" ++ tag ++ mid ++ close
regularChar = RC <$> (noneOf "`\n")

ifString s p = string s *> p
wrapping a w = w *> (a `endsWith` w)

endsWith a end = ((try end) *> pure []) <|> (a <:> endsWith a end)

(<:>) a b = (:) <$> a <*> b

line = (many1 (noneOf "\n")) <* eol

newlines = pure () <* (many1 $ char '\n')
eol = eof <|> (newlines *> pure ())

headingFromInt 1 = H1
headingFromInt 2 = H2
headingFromInt 3 = H3
headingFromInt 4 = H4
headingFromInt x = error ("heading level " ++ (show x) ++ " not supported.")
