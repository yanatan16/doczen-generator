module Language.Doczen.Formatter (
  formatDocument
) where

import Control.Monad.Writer
import Language.Doczen.Types
import Data.Char (toLower)

type Formatter = Writer String

formatDocument :: Document -> String
formatDocument d = execWriter (document d)

document d = do
  tell "<!doctype html>"
  tagTell "html" $ header (docHeader d) >> body (docSections d)

header h = do
  tell "<head>"
  tagTell "title" (tell $ title h)
  tell "<link href=\"/styles/bootstrap.css\" rel=\"stylesheet\">"
  tell "<link href=\"/styles/style.css\" rel=\"stylesheet\">"
  tell "</head>"

body ss = tagTell "body" $ main ss >> bodyFooter

main ss = tagAttrTell "div" "class=\"prose\"" $
  tagAttrTell "div" "class=\"inner\"" $
    mapM_ section ss


section (Section repl is) = tagAttrTell "section" attrs (mapM_ item is)
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

item (Paragraph p) = tagTell "p" (tell p)
item (Code c) = tagTell "pre" (tell c)
item (RunnableCode c) = tagAttrTell "pre" "class=\"runnable\"" (tell c)
item (Heading hl c) = tagTell (headingTag hl) (tell c)

tagTell :: String -> Formatter a -> Formatter ()
tagTell t c = tagAttrTell t "" c

tagAttrTell :: String -> String -> Formatter a -> Formatter ()
tagAttrTell t a c = do
  tell $ "<" ++ t ++ (if length a > 0 then " " ++ a else "") ++ ">"
  c
  tell $ "</" ++ t ++ ">"

bodyFooter = do
  tell "<div class=\"repls\"></div>"
  tell "<script src=\"//cdn.jsdelivr.net/jquery/2.1.1/jquery.min.js\"></script>"
  -- tell "<script src=\"https://rawgit.com/runningskull/jq-console/master/jqconsole.min.js\"></script>"
  tell "<script src=\"https://rawgit.com/jeresig/jquery.hotkeys/master/jquery.hotkeys.js\"></script>"
  tell "<script src=\"//cdn.jsdelivr.net/underscorejs/1.6.0/underscore-min.js\"></script>"
  tell "<script src=\"/jqconsole.min.js\"></script>"
  tell "<script src=\"/rainbow.min.js\"></script>"
  tell "<script src=\"https://rawgit.com/runningskull/booter/master/booter.js\"></script>"
  tell "<script src=\"/js/main.js\"></script>"