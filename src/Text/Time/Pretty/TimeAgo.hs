{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Time.Pretty.TimeAgo
  ( daysAgo
  , daysAgoToDays
  , DaysAgo(..)
  , timeAgo
  , timeAgoToDiffTime
  , TimeAgo(..)
  ) where

import Data.Time
import Data.Validity
import GHC.Generics (Generic)

import Text.Time.Pretty.Constants

data DaysAgo =
  DaysAgo
    { daysAgoSign :: Ordering
    , daysAgoDays :: Integer
    , daysAgoWeeks :: Integer
    , daysAgoMonths :: Integer
    , daysAgoYears :: Integer
    }
  deriving (Show, Eq, Generic)

instance Validity DaysAgo where
  validate DaysAgo {..} =
    mconcat
      [ check
          (case daysAgoSign of
             EQ -> and [daysAgoDays == 0, daysAgoWeeks == 0, daysAgoMonths == 0, daysAgoYears == 0]
             _ -> any (> 0) [daysAgoDays, daysAgoWeeks, daysAgoMonths, daysAgoYears])
          "the sign makes sense"
      , check (daysAgoYears >= 0) "years are positive"
      , check (daysAgoMonths < 13) "months < 13"
      , check (daysAgoMonths >= 0) "months are positive"
      , check (daysAgoWeeks < 5) "weeks < 5"
      , check (daysAgoWeeks >= 0) "weeks are positive"
      , check (daysAgoDays < 7) "days < 7"
      , check (daysAgoDays >= 0) "days are positive"
      ]

daysAgo :: Integer -> DaysAgo
daysAgo i = DaysAgo {..}
  where
    totalDays = abs i
    daysAgoSign = compare i 0
    daysAgoYears = totalDays `div` approximateDaysPerYear
    daysLeftAfterYears = totalDays - daysAgoYears * approximateDaysPerYear
    daysAgoMonths = daysLeftAfterYears `div` approximateDaysPerMonth
    daysLeftAfterMonths = daysLeftAfterYears - daysAgoMonths * approximateDaysPerMonth
    daysAgoWeeks = daysLeftAfterMonths `div` daysPerWeek
    daysLeftAfterWeeks = daysLeftAfterMonths - daysAgoWeeks * daysPerWeek
    daysAgoDays = daysLeftAfterWeeks

daysAgoToDays :: DaysAgo -> Integer
daysAgoToDays DaysAgo {..} =
  daysAgoDays + 7 * daysAgoWeeks + 12 * daysAgoMonths + 356 * daysAgoYears

data TimeAgo =
  TimeAgo
    { signAgo :: !Ordering
    , weeksAgo :: !Integer
    , daysAgo :: !Integer
    , hoursAgo :: !Integer
    , minutesAgo :: !Integer
    , secondsAgo :: !Integer
    , picoSecondsAgo :: !Integer
    }
  deriving (Show, Eq, Generic)

instance Validity TimeAgo where
  validate TimeAgo {..} =
    mconcat
      [ check
          (case signAgo of
             EQ ->
               and
                 [ weeksAgo == 0
                 , daysAgo == 0
                 , hoursAgo == 0
                 , minutesAgo == 0
                 , secondsAgo == 0
                 , picoSecondsAgo == 0
                 ]
             _ -> any (> 0) [weeksAgo, daysAgo, hoursAgo, minutesAgo, secondsAgo, picoSecondsAgo])
          "the sign makes sense"
      , check (weeksAgo >= 0) "weeks are positive"
      , check (daysAgo < daysPerWeek) "days < 7"
      , check (daysAgo >= 0) "days are positive"
      , check (hoursAgo < hoursPerDay) "hours < 24"
      , check (hoursAgo >= 0) "hours are positive"
      , check (minutesAgo < minutesPerHour) "minutes < 60"
      , check (minutesAgo >= 0) "minutes are positive"
      , check (secondsAgo < secondsPerMinute) "seconds < 60"
      , check (secondsAgo >= 0) "seconds are positive"
      , check (picoSecondsAgo < picoSecondsPerSecond) "picoseconds < 1E12"
      , check (picoSecondsAgo >= 0) "picoseconds are positive"
      ]

timeAgo :: NominalDiffTime -> TimeAgo
timeAgo dt = TimeAgo {..}
  where
    signAgo = compare dt 0
    picoSecondsAgo = totalPicoSecondsAgo - picoSecondsPerSecond * totalSecondsAgo
    secondsAgo = totalSecondsAgo - secondsPerMinute * totalMinutesAgo
    minutesAgo = totalMinutesAgo - minutesPerHour * totalHoursAgo
    hoursAgo = totalHoursAgo - hoursPerDay * totalDaysAgo
    daysAgo = totalDaysAgo - daysPerWeek * totalWeeksAgo
    weeksAgo = totalWeeksAgo
    totalPicoSecondsAgo = floor $ absDt * fromIntegral (picoSecondsPerSecond :: Integer)
    totalSecondsAgo = floor absDt :: Integer
    totalMinutesAgo = floor $ absDt / fromIntegral (secondsPerMinute :: Integer)
    totalHoursAgo = floor $ absDt / fromIntegral (minutesPerHour * secondsPerMinute :: Integer)
    totalDaysAgo =
      floor $ absDt / fromIntegral (hoursPerDay * minutesPerHour * secondsPerMinute :: Integer)
    totalWeeksAgo =
      floor $
      absDt /
      fromIntegral (daysPerWeek * hoursPerDay * minutesPerHour * secondsPerMinute :: Integer)
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
     picoSecondsPerSecond * (secondsAgo + secondsPerMinute * (minutesAgo + minutesPerHour * (hoursAgo + hoursPerDay * (daysAgo + daysPerWeek * weeksAgo)))))
