module AoC.Parsing
  ( module Text.Parsec
  , Parser
  , nat, int) where

import Text.Parsec
import Text.Parsec.Text (Parser)
import Data.Char (digitToInt)


nat :: (Num a) => Parser a
nat = foldl (\a x -> 10*a + fromIntegral (digitToInt x)) 0 <$> many1 digit

int :: (Num a) => Parser a
int = option id (negate <$ char '-') <*> nat