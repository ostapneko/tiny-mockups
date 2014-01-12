module Mockups.Parsers.Img where

import          Control.Applicative

import           Data.Attoparsec.Char8

import           Mockups.Elements.Element
import           Mockups.Parsers.Common
import           Mockups.Parsers.Combinators

imgParser :: Parser SimpleAttr
imgParser = do
    attrs <- withOptsAttrs "img" $ do
        withAttrName "src" (ImgSrc <$> inQuotes parseString)
    return $ Img attrs
