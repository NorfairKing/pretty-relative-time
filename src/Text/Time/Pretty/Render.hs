{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Time.Pretty.Render
  ( renderDaysAgoAuto
  , renderTimeAgoAuto
  ) where

import Text.Time.Pretty.TimeAgo

renderDaysAgoAuto :: DaysAgo -> String
renderDaysAgoAuto DaysAgo {..} =
  case daysAgoSign of
    GT ->
      if | daysAgoYears > 0 ->
           unwords [show daysAgoYears, plural daysAgoYears "year" "years", "ago"]
         | daysAgoMonths > 0 ->
           unwords [show daysAgoMonths, plural daysAgoMonths "month" "months", "ago"]
         | daysAgoWeeks > 0 ->
           unwords [show daysAgoWeeks, plural daysAgoWeeks "week" "weeks", "ago"]
         | daysAgoDays == 1 -> "yesterday"
         | daysAgoDays > 0 -> unwords [show daysAgoDays, plural daysAgoDays "day" "days", "ago"]
         | otherwise -> "today"
    EQ -> "today"
    LT ->
      if | daysAgoYears > 0 -> unwords ["in", show daysAgoYears, plural daysAgoYears "year" "years"]
         | daysAgoMonths > 0 ->
           unwords ["in", show daysAgoMonths, plural daysAgoMonths "month" "months"]
         | daysAgoWeeks > 0 -> unwords ["in", show daysAgoWeeks, plural daysAgoWeeks "week" "weeks"]
         | daysAgoDays == 1 -> "tomorrow"
         | daysAgoDays > 0 -> unwords ["in", show daysAgoDays, plural daysAgoDays "day" "days"]
         | otherwise -> "today"

renderTimeAgoAuto :: TimeAgo -> String
renderTimeAgoAuto TimeAgo {..} =
  case signAgo of
    GT ->
      if | weeksAgo > 0 -> unwords [show weeksAgo, plural weeksAgo "week" "weeks", "ago"]
         | daysAgo > 0 -> unwords [show daysAgo, plural daysAgo "day" "days", "ago"]
         | hoursAgo > 0 -> unwords [show hoursAgo, plural hoursAgo "hour" "hours", "ago"]
         | minutesAgo > 0 -> unwords [show minutesAgo, plural minutesAgo "minute" "minutes", "ago"]
         | secondsAgo > 0 -> unwords [show secondsAgo, plural secondsAgo "second" "seconds", "ago"]
         | otherwise -> "just now"
    EQ -> "just now"
    LT ->
      if | weeksAgo > 0 -> unwords ["in", show weeksAgo, plural weeksAgo "week" "weeks"]
         | daysAgo > 0 -> unwords ["in", show daysAgo, plural daysAgo "day" "days"]
         | hoursAgo > 0 -> unwords ["in", show hoursAgo, plural hoursAgo "hour" "hours"]
         | minutesAgo > 0 -> unwords ["in", show minutesAgo, plural minutesAgo "minute" "minutes"]
         | secondsAgo > 0 -> unwords ["in", show secondsAgo, plural secondsAgo "second" "seconds"]
         | otherwise -> "just now"

plural :: Integral a => a -> String -> String -> String
plural 1 sing _ = sing
plural _ _ plur = plur
