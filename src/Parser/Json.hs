{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Parser.Json where

import Data.Aeson (Value, decodeStrict)
import Data.ByteString (ByteString)
import Text.RawString.QQ (r)

sectionJson :: ByteString
sectionJson =
  [r|
{
  "section": {"host": "wikipedia.org"},
  "whatisit": {"red": "intoothandclaw"}
}
|]

a = do
  let blah :: Maybe Value
      blah = decodeStrict sectionJson
  print blah