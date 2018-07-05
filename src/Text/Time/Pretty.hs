module Text.Time.Pretty
    ( prettyTimeAutoFromNow
    , prettyTimeAuto
    -- Helper functions
    , timeAgo
    , timeAgoToDiffTime
    -- * Helper Types
    , TimeAgo(..)
    -- * Rendering
    , renderTimeAgoAuto
    -- * Constants
    , picoSecondsPerSecond
    , secondsPerMinute
    , minutesPerHour
    , hoursPerDay
    , daysPerWeek
    ) where

import Data.Time

import Text.Time.Pretty.Constants
import Text.Time.Pretty.Render
import Text.Time.Pretty.TimeAgo

prettyTimeAutoFromNow :: UTCTime -> IO String
prettyTimeAutoFromNow before = do
    now <- getCurrentTime
    pure $ prettyTimeAuto now before

prettyTimeAuto :: UTCTime -> UTCTime -> String
prettyTimeAuto now before = renderTimeAgoAuto $ timeAgo $ diffUTCTime now before
