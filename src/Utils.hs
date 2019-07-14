module Utils (
  rev
)where

rev :: [a] -> [a]
rev = foldl (flip (:)) []