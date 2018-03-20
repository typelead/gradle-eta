module Main where

import Hello.Mod

import Data.Text (Text, append)
import Data.Text.Encoding (decodeUtf8)

combineTextByteString :: Text
combineTextByteString = append (decodeUtf8 helloByteString) helloText

main :: IO ()
main = do
  print combineTextByteString
  print "Hello Again!"
