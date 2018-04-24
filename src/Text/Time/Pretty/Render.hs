{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}

module Text.Time.Pretty.Render
    ( renderTimeAgoAuto
    ) where

import Text.Time.Pretty.TimeAgo

renderTimeAgoAuto :: TimeAgo -> String
renderTimeAgoAuto TimeAgo {..} =
    case signAgo of
        GT ->
            if | daysAgo > 0 ->
                   unwords [show daysAgo, plural daysAgo "day" "days", "ago"]
               | hoursAgo > 0 ->
                   unwords
                       [show hoursAgo, plural hoursAgo "hour" "hours", "ago"]
               | minutesAgo > 0 ->
                   unwords
                       [ show minutesAgo
                       , plural minutesAgo "minute" "minutes"
                       , "ago"
                       ]
               | secondsAgo > 0 ->
                   unwords
                       [ show secondsAgo
                       , plural secondsAgo "second" "seconds"
                       , "ago"
                       ]
               | otherwise -> "just now"
        EQ -> "just now"
        LT ->
            if | daysAgo > 0 ->
                   unwords ["in", show daysAgo, plural daysAgo "day" "days"]
               | hoursAgo > 0 ->
                   unwords ["in", show hoursAgo, plural hoursAgo "hour" "hours"]
               | minutesAgo > 0 ->
                   unwords
                       [ "in"
                       , show minutesAgo
                       , plural minutesAgo "minute" "minutes"
                       ]
               | secondsAgo > 0 ->
                   unwords
                       [ "in"
                       , show secondsAgo
                       , plural secondsAgo "second" "seconds"
                       ]
               | otherwise -> "just now"

plural :: Integral a => a -> String -> String -> String
plural 1 sing _ = sing
plural _ _ plur = plur
