module Language.Doczen.PrettyPrint (
  prettyPrintDocument
) where

import Text.PrettyPrint

import Language.Doczen.Types

prettyPrintDocument :: Document -> String
prettyPrintDocument = render . document

document (Document h ss) = (header h) <> separator <> body ss
separator = text "---\n"
header (Header t) = text t <> char '\n'
body ss = hcat $ map section ss
section (Section _ is) = hcat $ map item is
item (Heading hl c) = headingPounds hl <> char ' ' <> text c <> char '\n'
item (Code c) = text "```\n" <> text c <> text "```\n"
item (RunnableCode c) = text "```runnable\n" <> text c <> text "```\n"
item (Paragraph c) = text c <> char '\n'

headingPounds H1 = text "#"
headingPounds H2 = text "##"
headingPounds H3 = text "###"
headingPounds H4 = text "####"