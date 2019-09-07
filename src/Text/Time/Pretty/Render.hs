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
  case timeAgoSign of
    GT ->
      if | daysAgoToDays timeAgoDaysAgo == 1 -> "1 day ago"
         | daysAgoToDays timeAgoDaysAgo > 0 ->
           renderDaysAgoAuto (timeAgoDaysAgo {daysAgoSign = timeAgoSign})
         | timeAgoHours > 0 ->
           unwords [show timeAgoHours, plural timeAgoHours "hour" "hours", "ago"]
         | timeAgoMinutes > 0 ->
           unwords [show timeAgoMinutes, plural timeAgoMinutes "minute" "minutes", "ago"]
         | timeAgoSeconds > 0 ->
           unwords [show timeAgoSeconds, plural timeAgoSeconds "second" "seconds", "ago"]
         | otherwise -> "just now"
    EQ -> "just now"
    LT ->
      if | daysAgoToDays timeAgoDaysAgo == 1 -> "in 1 day"
         | daysAgoToDays timeAgoDaysAgo > 0 ->
           renderDaysAgoAuto (timeAgoDaysAgo {daysAgoSign = timeAgoSign})
         | timeAgoHours > 0 -> unwords ["in", show timeAgoHours, plural timeAgoHours "hour" "hours"]
         | timeAgoMinutes > 0 ->
           unwords ["in", show timeAgoMinutes, plural timeAgoMinutes "minute" "minutes"]
         | timeAgoSeconds > 0 ->
           unwords ["in", show timeAgoSeconds, plural timeAgoSeconds "second" "seconds"]
         | otherwise -> "just now"

plural :: Integral a => a -> String -> String -> String
plural 1 sing _ = sing
plural _ _ plur = plur
