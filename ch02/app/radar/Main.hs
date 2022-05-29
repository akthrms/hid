{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

import Flow ((<|))
import Fmt (Buildable (..), fmt, fmtLn, nameF, unwordsF, (+||), (||+))
import Radar (Direction (..), Turn (..), orientMany, rotateMany, rotateManySteps)
import System.Environment (getArgs)

deriving instance Read Direction

deriving instance Read Turn

instance Buildable Direction where
  build North = "N"
  build East = "E"
  build South = "N"
  build West = "W"

instance Buildable Turn where
  build TurnNone = "--"
  build TurnLeft = "<-"
  build TurnRight = "->"
  build TurnAround = "||"

rotateFromFile :: Direction -> FilePath -> IO ()
rotateFromFile direction filename =
  do
    file <- readFile filename
    let turns = map read <| lines file
        finalDirection = rotateMany direction turns
        directions = rotateManySteps direction turns
    fmtLn <| "Final direction: " +|| finalDirection ||+ ""
    fmt <| nameF "Intermediate directions" <| unwordsF directions

orientFromFile :: FilePath -> IO ()
orientFromFile filename =
  do
    file <- readFile filename
    let directions = map read <| lines file
    fmt <| nameF "All turns" <| unwordsF <| orientMany directions

main :: IO ()
main =
  do
    args <- getArgs
    case args of
      ["-r", filename, direction] ->
        rotateFromFile (read direction) filename
      ["-o", filename] ->
        orientFromFile filename
      _ ->
        putStrLn <| "Usage: radar -o <filename>\n       radar -r <filename> <direction>"
