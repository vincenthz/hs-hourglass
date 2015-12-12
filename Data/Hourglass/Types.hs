{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Data.Hourglass.Types
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
--
-- Basic times units and types.
--
-- While pratically some units could hold infinite values, for practical
-- and efficient purpose they are limited to int64 types for seconds
-- and int types for years.
--
-- Most units use the unix epoch referential, but by no means reduce portability.
-- the unix referential works under the Windows platform or any other platforms.
--
-- This module will be depreciated in favor of Time.Types
--
module Data.Hourglass.Types
    ( module Time.Types
    ) where

import Time.Types
