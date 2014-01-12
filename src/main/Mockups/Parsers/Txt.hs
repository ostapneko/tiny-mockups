module Mockups.Parsers.Txt where

import          Control.Applicative

import           Data.Attoparsec.Char8

import           Mockups.Parsers.Combinators
import           Mockups.Parsers.Common
import           Mockups.Elements.Element

txtParser :: Parser SimpleAttr
txtParser = do
    attrs <- withOptsAttrs "txt" $ do
            withAttrName "content" (TxtContent <$> inQuotes parseString)
        <|> withAttrName "color" (TxtColor <$> parseColor)
    return $ Txt attrs
