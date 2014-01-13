module Mockups.Parsers.Element where

import           Control.Applicative

import           Data.Attoparsec.Char8

import           Mockups.Elements.Element
import           Mockups.Parsers.Common
import           Mockups.Parsers.Img
import           Mockups.Parsers.Txt
import           Mockups.Parsers.Box

eltParser :: IndentLevel -> Parser Element
eltParser lvl = do
        simpleEltParser lvl
    <|> containerParser lvl

containerParser :: IndentLevel -> Parser Element
containerParser lvl = do
    indentParser lvl
    containerAttr <- boxParser
    endOfLine
    children <- many $ eltParser (lvl + 1)
    return $ Container containerAttr children

simpleEltParser:: IndentLevel -> Parser Element
simpleEltParser lvl = do
        indentParser lvl
        simpleAttr <- imgParser <|> txtParser
        endOfLine
        return $ SimpleElement simpleAttr
