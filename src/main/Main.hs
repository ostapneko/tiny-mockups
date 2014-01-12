import Data.Attoparsec.Char8
import Data.Monoid
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

import Mockups.Parsers

main = do
    content <- BS.readFile "memories.mkp"
    let parser = many (eltParser 0)
    print $ parseOnly parser (content <> "\n")
