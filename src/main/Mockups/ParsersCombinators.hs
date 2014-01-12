module Mockups.Parsers where

import Data.Attoparsec.Char8
import qualified Data.HashMap.Strict as M
import Control.Applicative
import qualified Data.ByteString.Char8 as BS

type IndentLevel = Int

type AttrMap a = M.HashMap BS.ByteString a

data Color
    = Black | Blue
    deriving (Eq, Show)

data Element
    = SimpleElement SimpleAttr
    | Container ContainerAttr [Element]
    deriving (Eq, Show)

data SimpleAttr
    = Nav
    | Img (AttrMap ImgAttr)
    | Txt (AttrMap TxtAttr)
    deriving (Eq, Show)

data TxtAttr
    = TxtContent BS.ByteString
    | TxtColor Color
    deriving (Eq, Show)

data ImgAttr
    = ImgSrc BS.ByteString
    deriving (Eq, Show)

data ContainerAttr
    = Vbox (AttrMap VboxAttr)
    deriving (Eq, Show)

data VboxAttr
    = VboxSize Int
    deriving (Eq, Show)

eltParser :: IndentLevel -> Parser Element
eltParser lvl = do
        simpleEltParser lvl
    <|> containerParser lvl

simpleEltParser:: IndentLevel -> Parser Element
simpleEltParser lvl = do
        indentParser lvl
        simpleAttr <- imgParser <|> txtParser
        endOfLine
        return $ SimpleElement simpleAttr

imgParser :: Parser SimpleAttr
imgParser = do
    attrs <- withOptsAttrs "img" $ do
        withAttrName "src" (ImgSrc <$> inQuotes parseString)
    return $ Img attrs

txtParser :: Parser SimpleAttr
txtParser = do
    attrs <- withOptsAttrs "txt" $ do
            withAttrName "content" (TxtContent <$> inQuotes parseString)
        <|> withAttrName "color" (TxtColor <$> parseColor)
    return $ Txt attrs

parseColor :: Parser Color
parseColor = do
        (string "black" >> return Black)
    <|> (string "blue" >> return Blue)

withAttrName :: BS.ByteString
             -> Parser a
             -> Parser (BS.ByteString, a)
withAttrName name parse = do
    string name
    skipSpace
    char '='
    skipSpace
    value <- parse
    return (name, value)

withOptsAttrs :: BS.ByteString -> Parser (BS.ByteString, a)
              -> Parser (M.HashMap BS.ByteString a)
withOptsAttrs name parseAttr = do
    withAttrs name parseAttr <|> (string name >> return M.empty)

withAttrs :: BS.ByteString -> Parser (BS.ByteString, a)
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
inQuotes parse = do
    char '"'
    value <- parse
    char '"'
    return value

parseString :: Parser BS.ByteString
parseString = takeTill (=='"')

parseAttrSep :: Parser ()
parseAttrSep = do
    skipSpace
    char ','
    skipSpace

containerParser :: IndentLevel -> Parser Element
containerParser lvl = do
    indentParser lvl
    containerAttr <- vboxParser
    endOfLine
    children <- many $ eltParser (lvl + 1)
    return $ Container containerAttr children

vboxParser :: Parser ContainerAttr
vboxParser = do
    attrs <- withOptsAttrs "vbox" $ do
        withAttrName "size" (VboxSize <$> decimal)
    return $ Vbox attrs

indentParser :: IndentLevel -> Parser ()
indentParser lvl = count (lvl * 2) (char ' ') >> return ()
