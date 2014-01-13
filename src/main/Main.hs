import          Control.Applicative

import           Data.Attoparsec.Char8
import qualified Data.List             as L
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS

import           Text.Blaze.Html5 hiding (head)
import           Text.Blaze.Html5.Attributes
import           Text.Blaze.Html.Renderer.Pretty
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Mockups.Parsers.Element
import           Mockups.Elements.Element

import           System.Environment

main :: IO ()
main = do
    args <- getArgs
    content <- BS.readFile $ head args
    let parser = many (eltParser 0)
    case parseOnly parser (content <> "\n") of
        Right elts -> do
            let content = mconcat $ L.map writeHtml elts
            putStrLn $ renderHtml $ docTypeHtml content
        Left err   -> print err
