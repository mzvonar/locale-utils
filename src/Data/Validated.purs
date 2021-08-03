module Data.Validated where

data Validated a b
  = Valid a
  | Invalid a { errors :: Array b }