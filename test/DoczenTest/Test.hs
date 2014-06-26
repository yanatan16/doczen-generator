module DoczenTest.Test (tests) where

import Test.Framework (testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)

--import Test.QuickCheck
import Test.HUnit

import Language.Doczen.Types
import Language.Doczen.Formatter
import Language.Doczen.Parser
import Language.Doczen.PrettyPrint

import DoczenTest.Types ()

tests = [
        testGroup "Edge Case Parsing" [
          testCase "nothing" case_parse_nothing,
          testCase "no-title" case_parse_no_title,
          testCase "just-title" case_parse_just_title,
          testCase "empty-section" case_parse_empty_section,
          testCase "empty-sections" case_parse_empty_sections,
          testCase "no-eol-at-eof" case_parse_no_eol_at_eof
        ],
        testGroup "Edge Case Formatting" [
          testCase "nothing" case_format_nothing,
          testCase "just-title" case_format_just_title,
          testCase "empty-sections" case_format_empty_sections
        ],
        testGroup "Edge Case PrettyPrint" [
          testCase "nothing" case_pp_nothing,
          testCase "just-title" case_pp_just_title,
          testCase "empty-sections" case_pp_empty_sections
        ],
        testGroup "Pretty Print And Parse" [
          testProperty "ppp" prop_pp_parse
        ]
        --, testGroup "Format Valid HTML" [
        --  testProperty "format-valid-html" prop_format_valid_html
        --]
  ]

case_parse_nothing = testParse "Empty" "" $ Document (Header "") []
case_parse_no_title = testParse "No Title" "---\n# Heading\n" $ Document (Header "") [Section True [Heading H1 "Heading"]]
case_parse_just_title = testParse "Just Title" "Title\n" $ Document (Header "Title") []
case_parse_empty_section = testParse "Empty Section" "Title\n---\n---" $ Document (Header "Title") [Section True []]
case_parse_empty_sections = testParse "Empty Sections" "Title\n---\n---\n```\ncode\n```\n" $ Document (Header "Title") [Section True [], Section True [Code "code\n"]]
case_parse_no_eol_at_eof = testParse "No EOL at EOF" "Title\n---\n## Heading" $ Document (Header "Title") [Section True [Heading H2 "Heading"]]

case_format_nothing = assertEqual "Empty Doc" emptyDoc $ formatDocument (Document (Header "") [])
  where emptyDoc = formatPrefix ++ formatHead "" ++ formatBody "" ++ formatPostfix
case_format_just_title = assertEqual "Just Title" justTitle $ formatDocument (Document (Header "Title") [])
  where justTitle = formatPrefix ++ formatHead "Title" ++ formatBody "" ++ formatPostfix
case_format_empty_sections = assertEqual "Empty Sections" emptySecs $ formatDocument (Document (Header "") [Section True [], Section True []])
  where emptySecs = formatPrefix ++ formatHead "" ++ formatBody "<section class=\"has-repl\"></section><section class=\"has-repl\"></section>" ++ formatPostfix

case_pp_nothing = assertEqual "Empty Doc" emptyDoc $ prettyPrintDocument (Document (Header "") [])
  where emptyDoc = "---\n"
case_pp_just_title = assertEqual "Just Title" justTitle $ prettyPrintDocument (Document (Header "Title") [])
  where justTitle = "Title\n---\n"
case_pp_empty_sections = assertEqual "Empty Sections" emptySecs $ prettyPrintDocument (Document (Header "") [Section True [], Section True []])
  where emptySecs = "---\n---\n---\n"

prop_pp_parse doc = case parseDocument (prettyPrintDocument doc) of
  Left err -> False
  Right doc2 -> doc == doc2


testParse r s d = case parseDocument s of
  Left err -> assertFailure $ r ++ ": Error on parse: " ++ (show err)
  Right doc -> assertEqual (r ++ ": Document equality") d doc


formatPrefix = "<!doctype html><html>"
formatHead ttl = "<head><title>" ++ ttl ++ "</title><link href=\"/styles/bootstrap.css\" rel=\"stylesheet\"><link href=\"/styles/style.css\" rel=\"stylesheet\"></head>"
formatBodyPrefix = "<body><div class=\"prose\"><div class=\"inner\">"
formatBodyPostfix = "</div></div><div class=\"repls\"></div><script src=\"//cdn.jsdelivr.net/jquery/2.1.1/jquery.min.js\"></script><script src=\"https://rawgit.com/jeresig/jquery.hotkeys/master/jquery.hotkeys.js\"></script><script src=\"//cdn.jsdelivr.net/underscorejs/1.6.0/underscore-min.js\"></script><script src=\"/jqconsole.min.js\"></script><script src=\"/rainbow.min.js\"></script><script src=\"https://rawgit.com/runningskull/booter/master/booter.js\"></script><script src=\"/js/main.js\"></script></body>"
formatBody s = formatBodyPrefix ++ s ++ formatBodyPostfix
formatPostfix = "</html>"