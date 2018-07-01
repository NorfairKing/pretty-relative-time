{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Time.Pretty.TimeAgo
    ( timeAgo
    , timeAgoToDiffTime
    , TimeAgo(..)
    ) where

import Data.Time
import Data.Validity
import GHC.Generics (Generic)

import Text.Time.Pretty.Constants

data TimeAgo = TimeAgo
    { signAgo :: Ordering
    , weeksAgo :: Integer
    , daysAgo :: Integer
    , hoursAgo :: Integer
    , minutesAgo :: Integer
    , secondsAgo :: Integer
    , picoSecondsAgo :: Integer
    } deriving (Show, Eq, Generic)

instance Validity TimeAgo where
    isValid = isValidByValidating
    validate TimeAgo {..} =
        mconcat
            [ (case signAgo of
                   EQ ->
                       and
                           [ weeksAgo == 0
                           , daysAgo == 0
                           , hoursAgo == 0
                           , minutesAgo == 0
                           , secondsAgo == 0
                           , picoSecondsAgo == 0
                           ]
                   _ ->
                       any
                           (> 0)
                           [ weeksAgo
                           , daysAgo
                           , hoursAgo
                           , minutesAgo
                           , secondsAgo
                           , picoSecondsAgo
                           ]) <?@>
              "the sign makes sense"
            , (weeksAgo >= 0) <?@> "weeks are positive"
            , (daysAgo < daysPerWeek) <?@> "days < 7"
            , (daysAgo >= 0) <?@> "days are positive"
            , (hoursAgo < hoursPerDay) <?@> "hours < 24"
            , (hoursAgo >= 0) <?@> "hours are positive"
            , (minutesAgo < minutesPerHour) <?@> "minutes < 60"
            , (minutesAgo >= 0) <?@> "minutes are positive"
            , (secondsAgo < secondsPerMinute) <?@> "seconds < 60"
            , (secondsAgo >= 0) <?@> "seconds are positive"
            , (picoSecondsAgo < picoSecondsPerSecond) <?@> "picoseconds < 1E12"
            , (picoSecondsAgo >= 0) <?@> "picoseconds are positive"
            ]

timeAgo :: NominalDiffTime -> TimeAgo
timeAgo dt = TimeAgo {..}
  where
    signAgo = compare dt 0
    picoSecondsAgo =
        totalPicoSecondsAgo - picoSecondsPerSecond * totalSecondsAgo
    secondsAgo = totalSecondsAgo - secondsPerMinute * totalMinutesAgo
    minutesAgo = totalMinutesAgo - minutesPerHour * totalHoursAgo
    hoursAgo = totalHoursAgo - hoursPerDay * totalDaysAgo
    daysAgo = totalDaysAgo - daysPerWeek * totalWeeksAgo
    weeksAgo = totalWeeksAgo
    totalPicoSecondsAgo =
        floor $ absDt * fromIntegral (picoSecondsPerSecond :: Integer)
    totalSecondsAgo = floor absDt :: Integer
    totalMinutesAgo = floor $ absDt / fromIntegral (secondsPerMinute :: Integer)
    totalHoursAgo =
        floor $
        absDt / fromIntegral (minutesPerHour * secondsPerMinute :: Integer)
    totalDaysAgo =
        floor $
        absDt /
        fromIntegral
            (hoursPerDay * minutesPerHour * secondsPerMinute :: Integer)
    totalWeeksAgo =
        floor $
        absDt /
        fromIntegral
            (daysPerWeek * hoursPerDay * minutesPerHour * secondsPerMinute :: Integer)
    absDt = abs dt

timeAgoToDiffTime :: TimeAgo -> NominalDiffTime
timeAgoToDiffTime TimeAgo {..} =
    (/ fromIntegral (picoSecondsPerSecond :: Integer)) $
    realToFrac $
    (case signAgo of
         EQ -> const 0
         GT -> id
         LT -> negate)
        (picoSecondsAgo +
         picoSecondsPerSecond *
         (secondsAgo + 60 * (minutesAgo + 60 * (hoursAgo + 24 *( daysAgo + 7 * weeksAgo ) ))))
