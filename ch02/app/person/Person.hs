{-# LANGUAGE OverloadedStrings #-}

module Person where

import qualified Data.String as DataString

data Person
  = Person String (Maybe Int)

instance DataString.IsString Person where
  fromString name = Person name Nothing

homer :: Person
homer =
  Person "Homer Simpson" (Just 39)

spj :: Person
spj =
  "Simon Peyton Jones"
