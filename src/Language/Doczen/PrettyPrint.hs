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
section (Section opts is) = sectionOpts opts <> items is
sectionOpts = hcat . map sectionOpt
sectionOpt (NoAttachedRepl) = text "!norepl\n"
sectionOpt (SectionId i) = text $ "!id: " ++ i ++ "\n"
items = hcat . map item
item (Heading _ []) = empty
item (Heading hl c) = headingPounds hl <+> enhancedText c <> char '\n'
item (Code os c) = text "```" <> codeOptions os <> char '\n' <> codeText c <> text "```\n"
item (Paragraph []) = empty
item (Paragraph c) = enhancedText c <> char '\n'

codeOptions = hcat . map codeOption
codeOption Runnable = char '>'
codeOption Hidden = char '~'
codeOption (CodeId i) = text $ " (" ++ i ++ ")"

codeText = hcat . map codeTextNode
codeTextNode (RCC c) = char c
codeTextNode (Blanks NormalBlank) = text "____"
codeTextNode (Blanks LargeBlank) = text "______"

enhancedText = hcat . map enhancedTextNode
enhancedTextNode (RC c) = char c
enhancedTextNode (Small et) = "::" `wrapped` enhancedText et
enhancedTextNode (Em et) = "__" `wrapped` enhancedText et
enhancedTextNode (Strong et) = "**" `wrapped` enhancedText et
enhancedTextNode (InlineCode s) = "`" `wrapped` text s
enhancedTextNode (Tt et) = "++" `wrapped` enhancedText et
enhancedTextNode (Smile) = text ":)"
enhancedTextNode (SmileP) = text ":P"
enhancedTextNode (Link et s) = char '[' <> enhancedText et <> text "](" <> text s <> char ')'
enhancedTextNode (Html s) = text s

wrapped s t = text s <> t <> text s

headingPounds H1 = text "#"
headingPounds H2 = text "##"
headingPounds H3 = text "###"
headingPounds H4 = text "####"