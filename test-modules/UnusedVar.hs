module UnusedVar where

test :: Int
test = result
  where
    unused = 42
    result = 1
