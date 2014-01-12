module Mockups.Elements.Txt where

import qualified Data.ByteString.Char8 as BS
import           Mockups.Elements.Common

data TxtAttr
    = TxtContent BS.ByteString
    | TxtColor Color
    deriving (Eq, Show)
