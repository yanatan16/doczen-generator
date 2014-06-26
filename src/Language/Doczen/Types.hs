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
  sectionId :: String,
  items :: [Item]
} deriving (Show, Eq)

-- An item in a section
            -- A heading inside a section
data Item = Heading HeadingLevel EnhancedText
            -- A paragraph of information
          | Paragraph EnhancedText
            -- An piece of code
          | Code CodeOptions CodeText
     deriving (Show, Eq)

data HeadingLevel = H1 | H2 | H3 | H4
  deriving (Show, Eq)

data CodeOptions = CodeOptions Runnable Hidden
data Runnable = Static | Runnable
data Hidden = Shown | Hidden String

type EnhancedText = [EnhancedTextNode]
data EnhancedTextNode = Regular String
                      | Small EnhancedText
                      | Em EnhancedText
                      | Strong EnhancedText
                      | InlineCode String
                      | Tt EnhancedText
                      | Smile
                      | SmileP
                      | Link EnhancedText String
                      | Html String
  deriving (Show, Eq)