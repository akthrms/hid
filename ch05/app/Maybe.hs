module Maybe where

import Flow ((<|))
import Text.Read (readMaybe)

double1 :: (Num a, Read a) => String -> Maybe a
double1 str =
  case readMaybe str of
    Just x -> Just (2 * x)
    Nothing -> Nothing

double2 :: (Num a, Read a) => String -> Maybe a
double2 str =
  (2 *) `fmap` readMaybe str

plus :: (Num a, Read a) => String -> String -> Maybe a
plus s1 s2 =
  (+) <$> readMaybe s1 <*> readMaybe s2

type Name = String

type Phone = String

type Location = String

type PhoneNumbers = [(Name, Phone)]

type Locations = [(Phone, Location)]

locateByName :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName phoneNumbers locations name =
  lookup name phoneNumbers >>= flip lookup locations

locateByName' :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName' phoneNumbers locations name =
  case lookup name phoneNumbers of
    Just phoneNumber -> lookup phoneNumber locations
    Nothing -> Nothing

locateByName'' :: PhoneNumbers -> Locations -> Name -> Maybe Location
locateByName'' phoneNumbers locations name =
  do
    phoneNumber <- lookup name phoneNumbers
    lookup phoneNumber locations

main :: IO ()
main =
  do
    print <| (double2 "21" :: Maybe Int)
    print <| (double2 "yy" :: Maybe Int)
    print <| (plus "3" "5" :: Maybe Int)
    print <| (plus "3" "x" :: Maybe Int)
