-- |
-- Module      : Data.Hourglass.Internal
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- System lowlevel functions
--
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE EmptyDataDecls #-}

module Data.Hourglass.Internal
    ( dateTimeFromUnixEpochP
    , dateTimeFromUnixEpoch
    , systemGetTimezone
    , systemGetElapsed
    , systemGetElapsedP
    ) where

#ifdef WINDOWS
import Data.Hourglass.Internal.Win
#else
import Data.Hourglass.Internal.Unix
#endif
