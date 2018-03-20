{-# LANGUAGE OverloadedStrings #-}
module Hello.Mod (helloText, helloByteString) where

import Data.Text (Text)
import Data.ByteString (ByteString)

helloText :: Text
helloText = "Hello"

helloByteString :: ByteString
helloByteString = "Hello"
