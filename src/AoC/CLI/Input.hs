{-# LANGUAGE OverloadedStrings #-}
module AoC.CLI.Input
  ( getInput
  ) where

import Data.Text (Text)
import System.Environment (getEnv, lookupEnv)
import Advent


getInput :: Integer -> Integer -> IO Text
getInput year day = do
  session <- getEnv "AOC_SESSION"
  cache <- lookupEnv "AOC_CACHE"
  day' <- maybe (fail $ "invalid day: " ++ show day) return (mkDay day)
  let agent = AoCUserAgent "tobeannouncd/advent-haskell" "tobeannouncd@gmail.com"
      opts = (defaultAoCOpts agent year session) {_aCache = cache}
  runAoC_ opts $ AoCInput day'