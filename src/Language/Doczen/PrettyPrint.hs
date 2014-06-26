module Language.Doczen.PrettyPrint (
  prettyPrintDocument
) where

import Text.PrettyPrint

import Language.Doczen.Types

prettyPrintDocument :: Document -> String
prettyPrintDocument = render . document

document (Document h ss) = (header h) <> separator <> body ss
separator = text "---\n"
header (Header "") = empty
header (Header t) = text t <> char '\n'
body ss = sepJoin $ map section ss
sepJoin (s:t:ss) = s <> separator <> sepJoin (t:ss)
sepJoin [s] = if isEmpty s then separator else s
sepJoin [] = empty
section (Section True is) = hcat $ map item is
section (Section False is) = noReplIdentifier <> char '\n' <> section (Section True is)
item (Heading _ "") = empty
item (Heading hl c) = headingPounds hl <+> text c <> char '\n'
item (Code c) = text "```\n" <> text c <> text "```\n"
item (RunnableCode c) = text "```runnable\n" <> text c <> text "```\n"
item (Paragraph "") = empty
item (Paragraph c) = text c <> char '\n'

headingPounds H1 = text "#"
headingPounds H2 = text "##"
headingPounds H3 = text "###"
headingPounds H4 = text "####"

noReplIdentifier = text "{~ norepl ~}"