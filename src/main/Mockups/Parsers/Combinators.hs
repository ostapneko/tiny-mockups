module Mockups.Parsers.Combinators where

import          Control.Applicative

import           Data.Attoparsec.Char8
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as M

import           Mockups.Parsers.Common

withAttrName :: BS.ByteString
             -> Parser a
             -> Parser (BS.ByteString, a)
withAttrName name parser = do
    string name
    skipSpace
    char '='
    skipSpace
    value <- parser
    return (name, value)

withOptsAttrs :: BS.ByteString
              -> Parser (BS.ByteString, a)
              -> Parser (M.HashMap BS.ByteString a)
withOptsAttrs name parseAttr = do
        withAttrs name parseAttr
    <|> (string name >> return M.empty)

withAttrs :: BS.ByteString
          -> Parser (BS.ByteString, a)
          -> Parser (M.HashMap BS.ByteString a)
withAttrs name parseAttr = do
    string name
    skipSpace
    char '['
    skipSpace
    attrPairs <- parseAttr `sepBy` parseAttrSep
    skipSpace
    char ']'
    return $ M.fromList attrPairs

inQuotes :: Parser a -> Parser a
inQuotes parser = do
    char '"'
    value <- parser
    char '"'
    return value
