module Mockups.Parsers.Box where

import           Control.Applicative

import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS

import           Mockups.Parsers.Combinators
import           Mockups.Elements.Element

boxParser :: Parser ContainerAttr
boxParser = vboxParser <|> hboxParser

vboxParser :: Parser ContainerAttr
vboxParser = do
    Vbox <$> withOptsAttrs "vbox" parseSize

hboxParser :: Parser ContainerAttr
hboxParser = do
    Hbox <$> withOptsAttrs "hbox" parseSize

parseSize :: Parser (BS.ByteString, BoxAttr)
parseSize = withAttrName "size" (BoxSize <$> decimal)
