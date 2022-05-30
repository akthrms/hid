{-# LANGUAGE StandaloneDeriving #-}

import qualified Control.Monad as Monad
import Flow ((<|))
import Radar (Direction (..), Turn (..))
import qualified System.Random as Random
import qualified System.Random.Stateful as RandomS

deriving instance Ord Turn

instance Random.UniformRange Direction where
  uniformRM (low, high) rng =
    do
      result <- RandomS.uniformRM (fromEnum low :: Int, fromEnum high :: Int) rng
      pure <| toEnum result

instance Random.Uniform Direction where
  uniformM rng =
    RandomS.uniformRM (minBound, maxBound) rng

instance Random.UniformRange Turn where
  uniformRM (low, high) rng =
    do
      result <- RandomS.uniformRM (fromEnum low :: Int, fromEnum high :: Int) rng
      pure <| toEnum result

instance Random.Uniform Turn where
  uniformM rng =
    RandomS.uniformRM (minBound, maxBound) rng

uniformIO :: Random.Uniform a => IO a
uniformIO =
  Random.getStdRandom Random.uniform

uniformsIO :: Random.Uniform a => Int -> IO [a]
uniformsIO n =
  Monad.replicateM n uniformIO

randomTurns :: Int -> IO [Turn]
randomTurns =
  uniformsIO

randomDirections :: Int -> IO [Direction]
randomDirections =
  uniformsIO

writeRandomFile :: (Random.Random a, Show a) => Int -> (Int -> IO [a]) -> FilePath -> IO ()
writeRandomFile n generator filename =
  do
    xs <- generator n
    writeFile filename <| unlines <| map show xs
