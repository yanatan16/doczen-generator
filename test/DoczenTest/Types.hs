module DoczenTest.Types where

import Control.Applicative ((<$>),(<*>))
import Language.Doczen.Types
import Test.QuickCheck
import Data.Char (isSpace)

instance Arbitrary Document where
  arbitrary = Document <$> arbitrary <*> arbitrary

instance Arbitrary Header where
  arbitrary = Header <$> title'

instance Arbitrary Section where
  arbitrary = Section <$> arbitrary <*> arbitrary

instance Arbitrary Item where
  arbitrary = oneof
    [ Heading <$> arbitrary <*> heading'
    , Code <$> code'
    , RunnableCode <$> runnableCode'
    , Paragraph <$> paragraph'
    ]

instance Arbitrary HeadingLevel where
  arbitrary = elements [H1, H2, H3, H4]

title' = arbitrary
  `suchThat` noNewline
  `suchThat` doesntBeginWithSpaces

heading' = arbitrary
  `suchThat` nonEmpty
  `suchThat` notSpaces
  `suchThat` noNewline
  `suchThat` doesntBeginWithSpaces

paragraph' = arbitrary
  `suchThat` nonEmpty
  `suchThat` notSpaces
  `suchThat` noNewline
  `suchThat` doesntBeginWithSpaces
  `suchThat` doesntBeginWith "#"
  `suchThat` doesntBeginWith "---"

code' = arbitrary
  `suchThat` doesntBeginWithSpaces

runnableCode' = arbitrary
  `suchThat` doesntBeginWithSpaces

nonEmpty = not . null
notSpaces = any (\c -> (c /= ' ') && (c /= '\t'))
doesntBeginWithSpaces [] = True
doesntBeginWithSpaces s = (not . isSpace . head) s
noNewline = all (\c -> c /= '\n')
doesntBeginWith s = (s /=) . (take $ length s)