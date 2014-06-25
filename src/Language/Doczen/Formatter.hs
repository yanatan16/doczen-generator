module Language.Doczen.Formatter (
  formatDocument
) where

import Text.PrettyPrint
import Language.Doczen.Types
import Data.Char (toLower)

formatDocument :: Document -> String
formatDocument = render . document

document (Document h b) = text "<!doctype html>" <> tagged "html" (header h <> body b)

header (Header title) = tagged "head" inner
  where inner = tagged "title" (text title)
             <> text "<link href=\"/styles/bootstrap.css\" rel=\"stylesheet\">"
             <> text "<link href=\"/styles/style.css\" rel=\"stylesheet\">"

body ss = tagged "body" $ main ss <> bodyFooter

main ss = taggedAttr "div" "class=\"prose\"" $
  taggedAttr "div" "class=\"inner\"" $
    hcat $ map section ss

section (Section repl is) = taggedAttr "section" attrs (hcat $ map item is)
  where
    attrs = sectionId is ++ sectionClass
    sectionClass = if repl then "class=\"has-repl\"" else ""

sectionId :: [Item] -> String
sectionId its = case its of
  ((Heading _ c):_) -> "id=\"" ++ (idify c) ++ "\" "
  (_:rs)            -> sectionId rs
  []                -> ""

idify = map (toLower . replace)
  where
    replace ' ' = '-'
    replace '\'' = '-'
    replace '\"' = '-'
    replace c = c

item (Paragraph p) = tagged "p" (text p)
item (Code c) = tagged "pre" (text c)
item (RunnableCode c) = taggedAttr "pre" "class=\"runnable\"" (text c)
item (Heading hl c) = tagged (headingTag hl) (text c)

tagged :: String -> Doc -> Doc
tagged t c = taggedAttr t "" c

taggedAttr :: String -> String -> Doc -> Doc
taggedAttr t a c = char '<' <> text t <> attrs <> char '>' <> c <> text "</" <> text t <> char '>'
  where attrs = if length a > 0 then char ' ' <> text a else empty

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