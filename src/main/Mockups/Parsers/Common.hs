module Mockups.Parsers.Common where

import          Control.Applicative

import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as M

import           Mockups.Elements.Element

type IndentLevel = Int

indentParser :: IndentLevel -> Parser ()
indentParser lvl = count (lvl * 2) (char ' ') >> return ()

type AttrMap a = M.HashMap BS.ByteString a

parseColor :: Parser Color
parseColor = do
        (string "black" >> return Black)
    <|> (string "blue" >> return Blue)

parseString :: Parser BS.ByteString
parseString = takeTill (=='"')

parseAttrSep :: Parser ()
parseAttrSep = do
    skipSpace
    char ','
    skipSpace
