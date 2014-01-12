import Data.Attoparsec.Char8
import qualified Data.HashMap.Strict as M
import Test.Hspec

import Mockups.Elements.Element
import Mockups.Parsers.Element

main :: IO ()
main = hspec $ do
    describe "simple element parser" $ do
        it "parses image elements with attrs" $ do
            let exp = SimpleElement $ Img $ M.fromList [("src", ImgSrc "bla")]
            parseOnly (eltParser 0) "img [ src = \"bla\" ]\n" `shouldBe` Right exp
        it "parses image elements without attrs" $ do
            let exp = SimpleElement $ Img $ M.fromList []
            parseOnly (eltParser 0) "img\n" `shouldBe` Right exp
