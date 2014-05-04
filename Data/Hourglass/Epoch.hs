{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- |
-- Module      : Data.Hourglass.Epoch
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
-- Epoch tracking
--
module Data.Hourglass.Epoch
    (
    -- * computer time tracking with various epoch
      ElapsedSince(..)
    , ElapsedSinceP(..)
    -- * Epoch
    , Epoch(..)
    -- ** standard and usual epochs
    , UnixEpoch(..)
    , WindowsEpoch(..)
    ) where

import Data.Data
import Control.DeepSeq
import Data.Hourglass.Types
import Data.Hourglass.Time

-- | A number of seconds elapsed since an epoch.
newtype ElapsedSince epoch = ElapsedSince Seconds
    deriving (Show,Read,Eq,Ord,Num,Data,Typeable,NFData)

-- | A number of seconds and nanoseconds elapsed since an epoch.
data ElapsedSinceP epoch = ElapsedSinceP {-# UNPACK #-} !(ElapsedSince epoch)
                                         {-# UNPACK #-} !NanoSeconds
    deriving (Show,Read,Eq,Ord,Data,Typeable)

instance NFData (ElapsedSinceP e) where rnf e = e `seq` ()

instance Num (ElapsedSinceP e) where
    (ElapsedSinceP e1 ns1) + (ElapsedSinceP e2 ns2) = ElapsedSinceP (e1+e2) (ns1+ns2)
    (ElapsedSinceP e1 ns1) - (ElapsedSinceP e2 ns2) = ElapsedSinceP (e1-e2) (ns1-ns2)
    (ElapsedSinceP e1 ns1) * (ElapsedSinceP e2 ns2) = ElapsedSinceP (e1*e2) (ns1*ns2)
    negate (ElapsedSinceP e ns) = ElapsedSinceP (negate e) ns
    abs (ElapsedSinceP e ns)    = ElapsedSinceP (abs e) ns
    signum (ElapsedSinceP e ns) = ElapsedSinceP (signum e) ns
    fromInteger i          = ElapsedSinceP (ElapsedSince (fromIntegral i)) 0

-- FIXME instance Real (ElapsedSinceP e)

-- | epoch related.
--
-- We use the well known Unix epoch as the
-- reference timezone for doing conversion between epochs.
--
-- Each methods of this typeclass should not use the actual value,
-- but only get the information needed from the type itself.
class Epoch epoch where
    -- | The name of this epoch
    epochName :: epoch -> String

    -- | number of seconds of difference with 1st January 1970.
    --
    -- a negative number means that this epoch start before
    -- the unix epoch.
    epochDiffToUnix :: epoch -> Seconds

-- | Unix Epoch, starting 1st January 1970
data UnixEpoch = UnixEpoch
    deriving (Show,Eq)

instance Epoch UnixEpoch where
    epochName _ = "unix"
    epochDiffToUnix _ = 0

-- | Windows Epoch, starting 1st January 1601
data WindowsEpoch = WindowsEpoch
    deriving (Show,Eq)

instance Epoch WindowsEpoch where
    epochName _ = "windows"
    epochDiffToUnix _ = -11644473600

instance Epoch epoch => Timeable (ElapsedSince epoch) where
    timeGetElapsedP es = ElapsedP (Elapsed e) 0
      where ElapsedSince e = convertEpoch es :: ElapsedSince UnixEpoch
    timeGetElapsed   es = Elapsed e
      where ElapsedSince e = convertEpoch es :: ElapsedSince UnixEpoch
    timeGetNanoSeconds _ = 0

instance Epoch epoch => Time (ElapsedSince epoch) where
    timeFromElapsedP (ElapsedP (Elapsed e) _) =
        convertEpoch (ElapsedSince e :: ElapsedSince UnixEpoch)

instance Epoch epoch => Timeable (ElapsedSinceP epoch) where
    timeGetElapsedP es = ElapsedP (Elapsed e) ns
      where ElapsedSinceP (ElapsedSince e) ns = convertEpochP es :: ElapsedSinceP UnixEpoch
    timeGetNanoSeconds (ElapsedSinceP _ ns) = ns
instance Epoch epoch => Time (ElapsedSinceP epoch) where
    timeFromElapsedP (ElapsedP (Elapsed e) ns) = convertEpochP (ElapsedSinceP (ElapsedSince e) ns :: ElapsedSinceP UnixEpoch)

-- | Convert Elapsed seconds to another epoch with explicit epochs specified
convertEpochWith :: (Epoch e1, Epoch e2) => (e1,e2) -> ElapsedSince e1 -> ElapsedSince e2
convertEpochWith (e1,e2) (ElapsedSince s1) = ElapsedSince (s1 + diff)
  where diff = d1 - d2
        d1 = epochDiffToUnix e1
        d2 = epochDiffToUnix e2

-- | Convert Elapsed seconds to another epoch.
--
-- the actual epochs need to be known somehow by the context, otherwise this function
-- will yield a compilation errors as the epoch are not chosen.
--
-- If you want to force specific epoch conversion, use convertEpochWith
convertEpoch :: (Epoch e1, Epoch e2) => ElapsedSince e1 -> ElapsedSince e2
convertEpoch = convertEpochWith (undefined, undefined)

-- | Convert Precise Elapsed seconds to another epoch with explicit epochs specified
convertEpochPWith :: (Epoch e1, Epoch e2) => (e1,e2) -> ElapsedSinceP e1 -> ElapsedSinceP e2
convertEpochPWith es (ElapsedSinceP e1 n1) = ElapsedSinceP (convertEpochWith es e1) n1

-- | Convert Elapsed seconds to another epoch.
--
-- the actual epochs need to be known somehow by the context, otherwise this function
-- will yield a compilation errors as the epoch are not chosen.
--
-- If you want to force specific epoch conversion, use convertEpochWith
convertEpochP :: (Epoch e1, Epoch e2) => ElapsedSinceP e1 -> ElapsedSinceP e2
convertEpochP = convertEpochPWith (undefined, undefined)
