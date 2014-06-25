module Main where

import System.Environment

import Language.Doczen.Types
import Language.Doczen.Parser
import Language.Doczen.Formatter

import Text.Parsec (parse)

main = do
  args <- getArgs
  case args of
    [inf, outf] -> doczeninify inf outf
    [] -> error "requires argument of input file and output file"

doczeninify :: String -> String -> IO ()
doczeninify inf outf = do
  raw <- readFile inf
  let ret = parse documentParser inf raw
  case ret of
    Left err -> putStrLn (show err)
    Right doc -> writeDoc doc outf

writeDoc :: Document -> String -> IO ()
writeDoc doc outf = do
  writeFile outf (formatDocument doc)
  putStrLn $ "Wrote doczen output to " ++ outf