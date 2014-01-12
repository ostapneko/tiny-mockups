module Mockups.Parsers.Vbox where

import          Control.Applicative

import           Data.Attoparsec.Char8

import           Mockups.Parsers.Combinators
import           Mockups.Elements.Element

vboxParser :: Parser ContainerAttr
vboxParser = do
    attrs <- withOptsAttrs "vbox" $ do
        withAttrName "size" (VboxSize <$> decimal)
    return $ Vbox attrs
