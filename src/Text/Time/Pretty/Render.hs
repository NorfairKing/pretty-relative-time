{-# LANGUAGE DeriveGeneric #-}
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
            if | daysAgo > 0 -> unwords [show daysAgo, "days ago"]
               | hoursAgo > 0 -> unwords [show hoursAgo, "hours ago"]
               | minutesAgo > 0 -> unwords [show minutesAgo, "minutes ago"]
               | secondsAgo > 0 -> unwords [show secondsAgo, "seconds ago"]
               | otherwise -> "just now"
        EQ -> "just now"
        LT ->
            if | daysAgo > 0 -> unwords ["in", show daysAgo, "days"]
               | hoursAgo > 0 -> unwords ["in", show hoursAgo, "hours"]
               | minutesAgo > 0 -> unwords ["in", show minutesAgo, "minutes"]
               | secondsAgo > 0 -> unwords ["in", show secondsAgo, "seconds"]
               | otherwise -> "just now"
