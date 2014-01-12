module Mockups.Elements.Img where

import qualified Data.ByteString.Char8 as BS

data ImgAttr
    = ImgSrc BS.ByteString
    deriving (Eq, Show)
