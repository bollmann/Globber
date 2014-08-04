
module TestGlobber where

import Test.Hspec
import Control.Applicative
import Data.Monoid
import qualified Globber as G

main :: IO ()
main = hspec $ do
  describe "collapseStars" $ do
    it "collapses neighboring '*' symbols to just one of them" $
      G.collapseStars "***" `shouldBe` "*"
    it "doesn't do anything if there are no neighboring '*' symbols" $
      G.collapseStars "h*e*l*l*o*w*o*r*l*d" `shouldBe` "h*e*l*l*o*w*o*r*l*d"
    it "also it confirms this behavior for just a singleton \"*\" list" $
      G.collapseStars "*" `shouldBe` "*"

  describe "extractCharSet" $ do
    it "extracts all characters enclosed by [...]" $
      G.extractCharSet "aaa[abc]bbb" `shouldBe` (Just "aaa[abc]")
    it "also allows an escaped ], i.e., \\] within the glob match [...]" $
      G.extractCharSet "aaa[abc\\]d]bbb" `shouldBe` (Just "aaa[abc\\]d]")
    it "even allows an escaped ] as the *last* character in the glob match" $
      G.extractCharSet "ab[a\\]]ba" `shouldBe` (Just "ab[a\\]]")

  describe "buildCharChoices" $ do
    it "extracts the character choices from a given glob set correctly" $
      G.buildCharChoices "abc]d" `shouldBe` [ 'a', 'b', 'c', ']', 'd' ]


  describe "matchGlob" $ do
    it "matches exact literals correctly" $
       let literals = [ "abcde", "a]b", "-adf]ai1" ]
       in (getAll . mconcat . map All . getZipList $ (G.matchGlob <$> ZipList literals <*> ZipList literals)) `shouldBe` True

    it "matches escaped literals correctly" $
       let globs = [ "\\a\\b\\c\\d\\e", "\\[a]", "\\*\\*\\?", "\\\\a\\\\", "ab\\*ba", "ab\\[ba" ]
           literals = [ "abcde", "[a]", "**?", "\\a\\", "ab*ba", "ab[ba" ]
       in (getZipList $ (G.matchGlob <$> ZipList globs <*> ZipList literals)) `shouldBe` [True, True, True, True, True, True]

    it "matches sets correctly" $
       let globs = [ "[abcd]", "[a-d]", "[-abc]", "[abc-]", "[--]", "[---]", "[a-d-z]", "[z-a]", "ab[a\\]]ba" ]
           literalChoices = [ [ "a", "b", "c", "d" ]
                            , [ "a", "b", "c", "d" ]
                            , [ "-", "a", "b", "c" ]
                            , [ "-", "a", "b", "c" ]
                            , [ "-" ]
                            , [ "-" ]
                            , [ "a", "b", "c", "d", "-", "z" ]
                            , []
                            , [ "ababa", "ab]ba" ]
                      ]
       in zipWith (\glob literals -> getAll . mconcat . map All $ G.matchGlob <$> [glob] <*> literals) globs literalChoices
              `shouldBe` take (length globs) (repeat True)
