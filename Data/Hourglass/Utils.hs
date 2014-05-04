-- |
-- Module      : Data.Hourglass.Utils
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- some padding / formatting functions
--
module Data.Hourglass.Utils where

-- | pad a number to 2 digits
pad2 :: (Show a, Ord a, Num a, Integral a) => a -> String
pad2 v | v >= 100   = pad2 (v `mod` 100)
       | v >= 10    = show v
       | otherwise  = '0' : show v

-- | pad a number to 4 digits
pad4 :: (Show a, Ord a, Num a, Integral a) => a -> String
pad4 v | v >= 1000  = show v
       | v >= 100   = '0' : show v
       | v >= 10    = '0':'0' : show v
       | otherwise  = '0':'0':'0': show v
