module Dokan.Utils (
  splitBy,
) where

import Data.List (isPrefixOf)

splitBy :: String -> String -> [String]
splitBy delim = go [] ""
 where
  dlen :: Int
  dlen = length delim

  go :: [String] -> String -> String -> [String]
  go acc curr [] =
    case curr of
      [] -> reverse acc
      _ -> reverse (reverse curr : acc)
  go acc curr s@(c : cs)
    | delim `isPrefixOf` s =
        go (reverse curr : acc) "" (drop dlen s)
    | otherwise =
        go acc (c : curr) cs
