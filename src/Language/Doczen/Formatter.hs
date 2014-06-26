module Language.Doczen.Formatter (
  formatDocument
) where

import Text.PrettyPrint
import Language.Doczen.Types

formatDocument :: Document -> String
formatDocument = render . document

document (Document h b) = text "<!doctype html>" <> tagged "html" (header h <> body b)

header (Header ttl) = tagged "head" inner
  where inner = tagged "title" (text ttl)
             <> text "<link href=\"/styles/bootstrap.css\" rel=\"stylesheet\">"
             <> text "<link href=\"/styles/style.css\" rel=\"stylesheet\">"

body ss = tagged "body" $ main ss <> bodyFooter

main ss = taggedAttr "div" (attr "class" "prose") $
  taggedAttr "div" (attr "class" "inner") $
    hcat $ map section ss

section (Section opts is) = taggedAttr "section" attrs (hcat $ map item is)
  where
    attrs = replattr <+> idattr
    replattr = if all (/= NoAttachedRepl) opts then attr "class" "has-repl" else empty
    idattr = if any matchSectionId opts then attr "id" (unSectionId $ head $ filter matchSectionId opts) else empty
    matchSectionId (SectionId _) = True
    matchSectionId _ = False

unSectionId (SectionId i) = i
unSectionId _ = error "only section id"

item (Paragraph p) = tagged "p" (enhancedText p)
item (Code cs c) = taggedAttr "pre" coattrs (codeText c)
  where
    coattrs = hsep $ map coattr cs
    coattr Runnable = attr "class" "runnable"
    coattr Hidden = attr "style" "display: none;"
    coattr (CodeId i) = attr "id" i
item (Heading hl c) = tagged (headingTag hl) (enhancedText c)

codeText = hcat . map codeTextNode
codeTextNode (RCC c) = char c
codeTextNode (Blanks bs) = taggedAttr "span" (attr "contenteditable" "true") (text $ replicate (blankLength bs) ' ')

blankLength NormalBlank = 8
blankLength LargeBlank = 16

enhancedText = hcat . map enhancedTextNode
enhancedTextNode (RC c) = char c
enhancedTextNode (Small et) = tagged "small" $ enhancedText et
enhancedTextNode (Em et) = tagged "em" $ enhancedText et
enhancedTextNode (Strong et) = tagged "strong" $ enhancedText et
enhancedTextNode (InlineCode s) = tagged "code" $ text s
enhancedTextNode (Tt et) = tagged "tt" $ enhancedText et
enhancedTextNode (Smile) = taggedAttr "span" (attr "class" "smile") empty
enhancedTextNode (SmileP) = taggedAttr "span" (attr "class" "smile") $ char 'P'
enhancedTextNode (Link et s) = taggedAttr "a" (attr "href" s) $ enhancedText et
enhancedTextNode (Html s) = text s

attr :: String -> String -> Doc
attr n v = text n <> text "=\"" <> text v <> char '"'

tagged :: String -> Doc -> Doc
tagged t c = taggedAttr t empty c

taggedAttr :: String -> Doc -> Doc -> Doc
taggedAttr t a c = char '<' <> text t <+> a <> char '>' <> c <> text "</" <> text t <> char '>'

bodyFooter = text "<div class=\"repls\"></div>"
          <> text "<script src=\"//cdn.jsdelivr.net/jquery/2.1.1/jquery.min.js\"></script>"
          -- <> text "<script src=\"https://rawgit.com/runningskull/jq-console/master/jqconsole.min.js\"></script>"
          <> text "<script src=\"https://rawgit.com/jeresig/jquery.hotkeys/master/jquery.hotkeys.js\"></script>"
          <> text "<script src=\"//cdn.jsdelivr.net/underscorejs/1.6.0/underscore-min.js\"></script>"
          <> text "<script src=\"/jqconsole.min.js\"></script>"
          <> text "<script src=\"/rainbow.min.js\"></script>"
          <> text "<script src=\"https://rawgit.com/runningskull/booter/master/booter.js\"></script>"
          <> text "<script src=\"/js/main.js\"></script>"


headingTag :: HeadingLevel -> String
headingTag H1 = "h1"
headingTag H2 = "h2"
headingTag H3 = "h3"
headingTag H4 = "h4"