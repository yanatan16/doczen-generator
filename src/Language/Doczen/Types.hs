module Language.Doczen.Types where

-- A full document
data Document = Document {
  docHeader :: Header,
  docSections :: [Section]
} deriving (Show, Eq)

-- The HTML header of a Doczen page
data Header = Header {
  title :: String
} deriving (Show, Eq)

-- A section with an attached repl
data Section = Section {
  attachedRepl :: Bool,
  items :: [Item]
} deriving (Show, Eq)

-- An item in a section
            -- A heading inside a section
data Item = Heading { level :: HeadingLevel, unHeading :: String }
            -- A paragraph of information
          | Paragraph { unParagraph :: String }
            -- An un-runnable piece of code
          | Code { unCode :: String }
            -- A runnable piece of code
          | RunnableCode { unRunnableCode :: String }
     deriving (Show, Eq)


data HeadingLevel = H1 | H2 | H3 | H4
  deriving (Show, Eq)