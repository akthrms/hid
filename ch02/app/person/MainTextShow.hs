{-# LANGUAGE OverloadedStrings #-}

import Person
import qualified TextShow

instance TextShow.TextShow Person where
  showb (Person name Nothing) =
    TextShow.fromString name
  showb (Person name (Just age)) =
    TextShow.fromString name <> " (" <> TextShow.showb age <> ")"

main :: IO ()
main =
  do
    TextShow.printT homer
    TextShow.printT spj
