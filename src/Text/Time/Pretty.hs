{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Time.Pretty
    ( timeAgo
    , timeAgoToDiffTime
    , TimeAgo(..)
    , picoSecondsPerSecond
    , secondsPerMinute
    , minutesPerHour
    , hoursPerDay
    ) where

import Data.Time

import Text.Time.Pretty.Constants
import Text.Time.Pretty.TimeAgo

