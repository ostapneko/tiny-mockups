module Mockups.Elements.Element
    ( module Mockups.Elements.Common
    , module Mockups.Elements.Img
    , module Mockups.Elements.Txt
    , module Mockups.Elements.Box
    , AttrMap
    , Element(..)
    , ContainerAttr(..)
    , SimpleAttr(..)
    , writeHtml
    , ToOutput
    ) where

import           Data.Maybe
import           Data.Monoid
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as M
import qualified Data.List             as L

import           Text.Blaze.Html5
import           Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Mockups.Elements.Common
import Mockups.Elements.Box
import Mockups.Elements.Img
import Mockups.Elements.Txt

type AttrMap a = M.HashMap BS.ByteString a

class ToOutput a where
    writeHtml :: a -> Html

data Element
    = SimpleElement SimpleAttr
    | Container ContainerAttr [Element]
    deriving (Eq, Show)

instance ToOutput Element where
    writeHtml (SimpleElement attr) = writeHtml attr
    writeHtml (Container attr children) =
        let content = mconcat $ L.map writeHtml children
            boxClass = containerClass attr
        in H.div ! A.class_ boxClass $ content

data ContainerAttr
    = Vbox (AttrMap BoxAttr)
    | Hbox (AttrMap BoxAttr)
    deriving (Eq, Show)

containerClass :: ContainerAttr -> AttributeValue
containerClass (Vbox _) = "vbox"
containerClass (Hbox _) = "hbox"

data SimpleAttr
    = Nav
    | Img (AttrMap ImgAttr)
    | Txt (AttrMap TxtAttr)
    deriving (Eq, Show)

instance ToOutput SimpleAttr where
    writeHtml (Img attrMap) = case M.lookup "src" attrMap of
        Just (ImgSrc imgSrc) -> img ! src (unsafeByteStringValue imgSrc)
        Nothing              -> img
    writeHtml (Txt attrMap) =
        let html = case M.lookup "content" attrMap of
                       Just (TxtContent content) -> p . unsafeByteString $ content
                       _                         -> p ""
        in case M.lookup "color" attrMap of
            Just (TxtColor color) -> html ! (A.style $ "color: " <> (unsafeByteStringValue . colorToText $ color))
            _                     -> html
