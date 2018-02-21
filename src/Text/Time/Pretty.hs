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
import Data.Validity
import Debug.Trace
import GHC.Generics (Generic)

timeAgo :: NominalDiffTime -> TimeAgo
timeAgo dt = TimeAgo {..}
  where
    signAgo = compare dt 0
    picoSecondsAgo =
        totalPicoSecondsAgo - picoSecondsPerSecond * totalSecondsAgo
    secondsAgo = totalSecondsAgo - secondsPerMinute * totalMinutesAgo
    minutesAgo = totalMinutesAgo - minutesPerHour * totalHoursAgo
    hoursAgo = totalHoursAgo - hoursPerDay * totalDaysAgo
    daysAgo = totalDaysAgo
    totalPicoSecondsAgo =
        floor $ absDt * fromIntegral (picoSecondsPerSecond :: Integer)
    totalSecondsAgo = floor absDt :: Integer
    totalMinutesAgo = floor $ absDt / fromIntegral (secondsPerMinute :: Integer)
    totalHoursAgo =
        floor $
        absDt / (fromIntegral (minutesPerHour * secondsPerMinute :: Integer))
    totalDaysAgo =
        floor $
        absDt /
        (fromIntegral
             (hoursPerDay * minutesPerHour * secondsPerMinute :: Integer))
    absDt = abs dt

picoSecondsPerSecond :: Integral a => a
picoSecondsPerSecond = 10 ^ (12 :: Integer)

secondsPerMinute :: Integral a => a
secondsPerMinute = 60

minutesPerHour :: Integral a => a
minutesPerHour = 60

hoursPerDay :: Integral a => a
hoursPerDay = 24

timeAgoToDiffTime :: TimeAgo -> NominalDiffTime
timeAgoToDiffTime TimeAgo {..} =
    (/ fromIntegral picoSecondsPerSecond) $
    realToFrac $
    (case signAgo of
         EQ -> const 0
         GT -> id
         LT -> negate) $
    (picoSecondsAgo +
     picoSecondsPerSecond *
     (secondsAgo + 60 * (minutesAgo + 60 * (hoursAgo + 24 * daysAgo))))

data TimeAgo = TimeAgo
    { signAgo :: Ordering
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
                           [ daysAgo == 0
                           , hoursAgo == 0
                           , minutesAgo == 0
                           , secondsAgo == 0
                           , picoSecondsAgo == 0
                           ]
                   _ ->
                       any
                           (> 0)
                           [ daysAgo
                           , hoursAgo
                           , minutesAgo
                           , secondsAgo
                           , picoSecondsAgo
                           ]) <?@>
              "the sign makes sense"
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
