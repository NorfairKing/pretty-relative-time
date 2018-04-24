module Text.Time.Pretty.Constants
    ( picoSecondsPerSecond
    , secondsPerMinute
    , minutesPerHour
    , hoursPerDay
    ) where

picoSecondsPerSecond :: Integral a => a
picoSecondsPerSecond = 10 ^ (12 :: Integer)

secondsPerMinute :: Integral a => a
secondsPerMinute = 60

minutesPerHour :: Integral a => a
minutesPerHour = 60

hoursPerDay :: Integral a => a
hoursPerDay = 24
