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
import Numeric.Natural

import Text.Time.Pretty.Constants

data DaysAgo =
  DaysAgo
    { daysAgoSign :: Ordering
    , daysAgoYears :: Natural
    , daysAgoMonths :: Natural
    , daysAgoWeeks :: Natural
    , daysAgoDays :: Natural
    }
  deriving (Show, Eq, Generic)

instance Validity DaysAgo where
  validate da@DaysAgo {..} =
    mconcat
      [ genericValidate da
      , check
          (case daysAgoSign of
             EQ -> and [daysAgoDays == 0, daysAgoWeeks == 0, daysAgoMonths == 0, daysAgoYears == 0]
             _ -> any (> 0) [daysAgoDays, daysAgoWeeks, daysAgoMonths, daysAgoYears])
          "the sign makes sense"
      , check
          (daysAgoDays + daysPerWeek * daysAgoWeeks + approximateDaysPerMonth * daysAgoMonths <
           approximateDaysPerYear)
          "days, weeks and months do not sum to a year"
      , check (daysAgoMonths < 12) "months < 12"
      , check
          (daysAgoDays + daysPerWeek * daysAgoWeeks < approximateDaysPerMonth)
          "days and weeks do not sum to a month"
      , check (daysAgoWeeks < 5) "weeks < 5"
      , check (daysAgoDays < 7) "days < 7"
      ]

daysAgo :: Integer -> DaysAgo
daysAgo i = DaysAgo {..}
  where
    totalDays = abs i
    daysAgoSign = compare i 0
    daysAgoYears = fromInteger totalDays `div` fromInteger approximateDaysPerYear :: Natural
    daysLeftAfterYears =fromInteger totalDays - daysAgoYears * fromInteger approximateDaysPerYear :: Natural
    daysAgoMonths = daysLeftAfterYears `div` fromInteger approximateDaysPerMonth :: Natural
    daysLeftAfterMonths =
      daysLeftAfterYears - daysAgoMonths * fromInteger approximateDaysPerMonth :: Natural
    daysAgoWeeks = daysLeftAfterMonths `div` fromInteger daysPerWeek :: Natural
    daysLeftAfterWeeks = daysLeftAfterMonths - daysAgoWeeks * fromInteger daysPerWeek :: Natural
    daysAgoDays = daysLeftAfterWeeks

daysAgoToDays :: DaysAgo -> Integer
daysAgoToDays DaysAgo {..} =
  (case daysAgoSign of
     EQ -> const 0
     GT -> id
     LT -> negate) $
  toInteger $
  daysAgoDays + daysPerWeek * daysAgoWeeks + approximateDaysPerMonth * daysAgoMonths +
  approximateDaysPerYear * daysAgoYears

data TimeAgo =
  TimeAgo
    { timeAgoSign :: Ordering
    , timeAgoDaysAgo :: DaysAgo
    , timeAgoHours :: Natural
    , timeAgoMinutes :: Natural
    , timeAgoSeconds :: Natural
    , timeAgoPicoSeconds :: Natural
    }
  deriving (Show, Eq, Generic)

instance Validity TimeAgo where
  validate ta@TimeAgo {..} =
    mconcat
      [ genericValidate ta
      , check
          (case timeAgoSign of
             EQ ->
               and
                 [ daysAgoToDays timeAgoDaysAgo == 0
                 , timeAgoHours == 0
                 , timeAgoMinutes == 0
                 , timeAgoSeconds == 0
                 , timeAgoPicoSeconds == 0
                 ]
             _ ->
               any
                 (> 0)
                 [ fromInteger $ daysAgoToDays timeAgoDaysAgo
                 , timeAgoHours
                 , timeAgoMinutes
                 , timeAgoSeconds
                 , timeAgoPicoSeconds
                 ])
          "the sign makes sense"
      , check (daysAgoSign timeAgoDaysAgo /= LT) "The days ago are not negative"
      , check (timeAgoHours < hoursPerDay) "hours < 24"
      , check (timeAgoMinutes < minutesPerHour) "minutes < 60"
      , check (timeAgoSeconds < secondsPerMinute) "seconds < 60"
      , check (timeAgoPicoSeconds < picoSecondsPerSecond) "picoseconds < 1E12"
      ]

timeAgo :: NominalDiffTime -> TimeAgo
timeAgo dt = TimeAgo {..}
  where
    timeAgoSign = compare dt 0
    timeAgoPicoSeconds = fromInteger $ totalPicoSecondsAgo - picoSecondsPerSecond * totalSecondsAgo
    timeAgoSeconds = fromInteger $ totalSecondsAgo - secondsPerMinute * totalMinutesAgo
    timeAgoMinutes = fromInteger $ totalMinutesAgo - minutesPerHour * totalHoursAgo
    timeAgoHours = fromInteger $ totalHoursAgo - hoursPerDay * totalDaysAgo
    timeAgoDaysAgo = daysAgo totalDaysAgo
    totalPicoSecondsAgo = floor $ absDt * fromIntegral (picoSecondsPerSecond :: Integer)
    totalSecondsAgo = floor absDt :: Integer
    totalMinutesAgo = floor $ absDt / fromIntegral (secondsPerMinute :: Integer)
    totalHoursAgo = floor $ absDt / fromIntegral (minutesPerHour * secondsPerMinute :: Integer)
    totalDaysAgo =
      floor $ absDt / fromIntegral (hoursPerDay * minutesPerHour * secondsPerMinute :: Integer)
    absDt = abs dt

timeAgoToDiffTime :: TimeAgo -> NominalDiffTime
timeAgoToDiffTime TimeAgo {..} =
  (/ fromIntegral (picoSecondsPerSecond :: Integer)) $
  realToFrac $
  (case timeAgoSign of
     EQ -> const 0
     GT -> id
     LT -> negate)
    (timeAgoPicoSeconds +
     fromInteger picoSecondsPerSecond *
     (timeAgoSeconds +
      fromInteger secondsPerMinute *
      (timeAgoMinutes +
       fromInteger minutesPerHour *
       (timeAgoHours + fromInteger hoursPerDay * fromInteger (daysAgoToDays timeAgoDaysAgo)))))
