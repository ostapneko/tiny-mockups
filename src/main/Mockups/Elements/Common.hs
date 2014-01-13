module Mockups.Elements.Common where

import qualified Data.ByteString.Char8 as BS

data Color
    = Black | Blue
    deriving (Eq, Show)

colorToText :: Color -> BS.ByteString
colorToText Black = "black"
colorToText Blue = "blue"
