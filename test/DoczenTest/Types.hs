module DoczenTest.Types where

import Control.Applicative ((<$>),(<*>),pure)
import Language.Doczen.Types
import Test.QuickCheck
import Data.List (permutations, subsequences)

instance Arbitrary Document where
  arbitrary = Document <$> arbitrary <*> (arbitrary `suchThat` nonEmpty)
  shrink (Document h ss) = Document <$> shrink h <*> shrinkNonEmpty ss

instance Arbitrary Header where
  arbitrary = Header <$> charstring'
  shrink (Header _) = [Header ""]

instance Arbitrary Section where
  arbitrary = Section <$> sectionOptions' <*> (arbitrary `suchThat` nonEmpty)
  shrink (Section opts is) = Section <$> shrink opts <*> shrinkNonEmpty is

instance Arbitrary SectionOption where
  arbitrary = error "Not used"
  shrink _ = []

instance Arbitrary Item where
  arbitrary = oneof
    [ Heading <$> arbitrary <*> (enhancedtext' `suchThat` nonEmpty)
    , Code <$> codeOptions' <*> codetext'
    , Paragraph <$> paragraph'
    ]
  shrink (Heading hl et) = Heading hl <$> shrinkNonEmpty et
  shrink (Code co ct) = Code <$> shrink co <*> shrink ct
  shrink (Paragraph et) = Paragraph <$> shrinkNonEmpty et

instance Arbitrary CodeOption where
  arbitrary = error "Not Used"
  shrink _ = []

instance Arbitrary CodeTextNode where
  arbitrary = frequency
    [ (100, RCC <$> allchars)
    , (1, Blanks <$> arbitrary)
    ]

instance Arbitrary BlankSize where
  arbitrary = elements [ NormalBlank, LargeBlank ]

instance Arbitrary EnhancedTextNode where
  arbitrary = frequency
    [ (100, RC <$> allchars)
    , (1, Small <$> (enhancedtext' `suchThat` doesntContain isSmall))
    , (1, Em <$> (enhancedtext' `suchThat` doesntContain isEm))
    , (1, Strong <$> (enhancedtext' `suchThat` doesntContain isStrong))
    , (1, Tt <$> (enhancedtext' `suchThat` doesntContain isTt))
    , (1, InlineCode <$> charstring')
    , (1, pure Smile)
    , (1, pure SmileP)
    , (1, Link <$> enhancedtext' <*> (charstring' `suchThat` doesntContain (== ')')))
    , (1, Html <$> html')
    ]
  shrink (RC _) = []
  shrink (Small et) = Small <$> shrink et
  shrink (Em et) = Em <$> shrink et
  shrink (Strong et) = Strong <$> shrink et
  shrink (Tt et) = Tt <$> shrink et
  shrink (InlineCode _) = []
  shrink (Smile) = []
  shrink (SmileP) = []
  shrink (Link et s) = Link <$> shrink et <*> pure s
  shrink (Html _) = []

instance Arbitrary HeadingLevel where
  arbitrary = elements [H1, H2, H3, H4]

enhancedtext' = (arbitrary :: Gen EnhancedText)
   `suchThat` nonEmpty
   `suchThat` doesntBeginWith [RC ' ']
   `suchThat` doesntBeginWith [RC '\t']

codetext' = sized (\n -> promote ([RCC <$> endchars]
                                ++ replicate (n - 2) (arbitrary :: Gen CodeTextNode)
                                ++ [RCC <$> endchars]))
 `suchThat` noConsecutive isBlank

sectionOptions' = uniquelist
  [ pure NoAttachedRepl
  , SectionId <$> idfield'
  ]

codeOptions' = uniquelist
  [ pure Runnable
  , pure Hidden
  , CodeId <$> idfield'
  ]

paragraph' = enhancedtext'
  `suchThat` nonEmpty
  `suchThat` doesntBeginWith (map RC "---")
  `suchThat` doesntBeginWith [RC '#']
  `suchThat` doesntBeginWith [RC '\t']
  `suchThat` doesntBeginWith [RC '!']

html' = sized $ promote . (\n -> map pure "<tag>"
                              ++ replicate n allchars
                              ++ map pure "</tag>")

charstring' = sized $ promote . (\n -> endchars : (replicate (n - 2) allchars) ++ [endchars])

idfield' = sized $ promote . (\n -> loweralpha : replicate (n - 1) lowerdash)

nonEmpty = not . null
doesntBeginWith s = (s /=) . (take $ length s)
noConsecutive f = not . any (== (True, True)) . pairs . map f
doesntContain f = not . any f
pairs l = zip l (tail l)

isBlank (Blanks _) = True
isBlank _ = False

isSmall (Small _) = True
isSmall _ = False

isEm (Em _) = True
isEm _ = False

isTt (Tt _) = True
isTt _ = False

isStrong (Strong _) = True
isStrong _ = False

loweralpha = choose ('a', 'z')
lowerdash = frequency [ (26, choose ('a', 'z')), (1, elements ['-']) ]
allchars = frequency
  [ (260, choose ('a', 'z'))
  , (26, choose ('A', 'Z'))
  , (50, choose ('0', '9'))
  , (2, elements [' ','\t'])
  , (9, choose ('!', ')'))
  , (4, choose (',', '/'))
  , (6, choose (';', '@'))
  , (2, elements ['\\', '^'])
  , (4, choose ('{', '~'))
  ]
endchars = frequency
  [ (260, choose ('a', 'z'))
  , (26, choose ('A', 'Z'))
  , (50, choose ('0', '9'))
  ]

shrinkNonEmpty :: Arbitrary a => [a] -> [[a]]
shrinkNonEmpty a = [x | x <- shrink a, not (null x)]

uniquelist :: [Gen a] -> Gen [a]
uniquelist = oneof . map promote . concat . map permutations . subsequences