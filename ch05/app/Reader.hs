{-# LANGUAGE NamedFieldPuns #-}

import Control.Monad.Reader (Reader, when)
import qualified Control.Monad.Reader as R

newtype Config = Config {verbose :: Bool}

getConfiguration :: IO Config
getConfiguration =
  pure Config {verbose = True}

type ConfigM = Reader Config

work :: ConfigM ()
work =
  do
    doSomething

doSomething :: ConfigM ()
doSomething =
  do
    doSomethingSpecial

doSomethingSpecial :: ConfigM ()
doSomethingSpecial =
  do
    Config {verbose} <- R.ask
    when verbose beVerbose

beVerbose :: ConfigM ()
beVerbose =
  pure ()

silent :: Config -> Config
silent config =
  config {verbose = False}

doSomethingSpecialSilently :: ConfigM ()
doSomethingSpecialSilently =
  R.local silent doSomethingSpecial

main :: IO ()
main =
  do
    config <- getConfiguration
    let result = R.runReader work config
    print result
