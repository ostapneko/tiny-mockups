import          Control.Applicative

import           Data.Attoparsec.Char8
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS

import Mockups.Parsers.Element

main :: IO ()
main = do
    content <- BS.readFile "example.mkp"
    let parser = many (eltParser 0)
    print $ parseOnly parser (content <> "\n")
