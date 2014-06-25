module Main where

import System.Console.ArgParser

import Language.Doczen.Types
import Language.Doczen.Parser
import Language.Doczen.Formatter
import Language.Doczen.PrettyPrint

data DoczenCli = DoczenCli {
  infile :: String,
  outfile :: String,
  printType :: String
}

doczenCliParser = DoczenCli
  `parsedBy` reqPos "in"
  `andBy` optPos "" "out"
  `andBy` optFlag "html" "print"

main = do
  interface <- mkApp doczenCliParser
  runApp interface run

run :: DoczenCli -> IO ()
run cli = do
  doc <- readDoc (infile cli)
  let s = formatDoc (printType cli) doc
  writeDoc (outfile cli) s

readDoc :: String -> IO Document
readDoc fn = do
  raw <- readFile fn
  case parseDocument raw of
    Left err -> error (show err)
    Right doc -> return doc

formatDoc :: String -> Document -> String
formatDoc "html" = formatDocument
formatDoc "pp" = prettyPrintDocument
formatDoc _ = error "print type not supported"

writeDoc :: String -> String -> IO ()
writeDoc "" = putStr
writeDoc fn = writeFile fn