module Mockups.Elements.Element
    ( module Mockups.Elements.Common
    , module Mockups.Elements.Img
    , module Mockups.Elements.Txt
    , module Mockups.Elements.Vbox
    , AttrMap
    , Element(..)
    , ContainerAttr(..)
    , SimpleAttr(..)
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as M

import Mockups.Elements.Common
import Mockups.Elements.Img
import Mockups.Elements.Txt
import Mockups.Elements.Vbox

type AttrMap a =  M.HashMap BS.ByteString a

data Element
    = SimpleElement SimpleAttr
    | Container ContainerAttr [Element]
    deriving (Eq, Show)

data ContainerAttr
    = Vbox (AttrMap VboxAttr)
    deriving (Eq, Show)

data SimpleAttr
    = Nav
    | Img (AttrMap ImgAttr)
    | Txt (AttrMap TxtAttr)
    deriving (Eq, Show)
