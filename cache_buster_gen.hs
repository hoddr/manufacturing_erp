-- simple script to generate cache busting uuid for forced client-side updates
-- requires global cabal install --lib uuid

module Main where

import Data.UUID.V4 ( nextRandom )

main :: IO ()
main = nextRandom >>= print
