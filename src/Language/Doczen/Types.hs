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
data Section = Section [SectionOption] [Item]
  deriving (Show, Eq)
data SectionOption = NoAttachedRepl
                   | SectionId String
  deriving (Show, Eq)

-- An item in a section
            -- A heading inside a section
data Item = Heading HeadingLevel EnhancedText
            -- A paragraph of information
          | Paragraph EnhancedText
            -- An piece of code
          | Code [CodeOption] CodeText
     deriving (Show, Eq)

data HeadingLevel = H1 | H2 | H3 | H4
  deriving (Show, Eq)

data CodeOption = Runnable | Hidden | CodeId String
  deriving (Show, Eq)

type EnhancedText = [EnhancedTextNode]
data EnhancedTextNode = RC Char
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

type CodeText = [CodeTextNode]
data CodeTextNode = RCC Char
                  | Blanks BlankSize
  deriving (Show, Eq)

data BlankSize = NormalBlank | LargeBlank
  deriving (Show, Eq)

--instance Show EnhancedTextNode where
--  show (RC c) = show c
--  show (Small et) = "Small " ++ show et
--  show (Em et) = "Em " ++ show et
--  show (Strong et) = "Strong " ++ show et
--  show (InlineCode et) = "InlineCode " ++ show et
--  show (Tt et) = "Tt " ++ show et
--  show (Smile) = "Smile"
--  show (SmileP) = "SmileP"
--  show (Link et s) = "Link " ++ show et ++ show s
--  show (Html s) = "Html " ++ show s

--instance Show CodeTextNode where
--  show (RCC c) = show c
--  show (Blanks bs) = "Blanks " ++ (show bs)