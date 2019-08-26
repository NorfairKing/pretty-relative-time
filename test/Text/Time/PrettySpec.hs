{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Time.PrettySpec
  ( spec
  ) where

import Data.GenValidity.Time ()
import Test.Hspec
import Test.QuickCheck
import Test.Validity

import Text.Time.Pretty

instance GenUnchecked DaysAgo

instance GenValid DaysAgo where
  genValid = do
    sign <- genValid
    case sign of
      EQ -> pure $ DaysAgo EQ 0 0 0 0
      _ ->
        (DaysAgo sign <$> choose (0, 6) <*> choose (0, 5) <*> choose (0, 29) <*> choose (0, 364)) `suchThat`
        isValid

instance GenUnchecked TimeAgo

instance GenValid TimeAgo where
  genValid = do
    sign <- genValid
    case sign of
      EQ -> pure $ TimeAgo EQ 0 0 0 0 0
      _ ->
        (TimeAgo sign <$> choose (0, picoSecondsPerSecond) <*> choose (0, secondsPerMinute) <*>
         choose (0, minutesPerHour) <*>
         choose (0, hoursPerDay) <*>
         (abs <$> genValid)) `suchThat`
        isValid

spec :: Spec
spec = do
  eqSpec @TimeAgo
  genValidSpec @TimeAgo
  describe "timeAgo" $ do
    it "produces valid TimeAgo's" $ producesValidsOnValids timeAgo
    it "is the inverse of timeAgoToDiffTime" $ inverseFunctionsOnValid timeAgo timeAgoToDiffTime
  describe "timeAgoToDiffTime" $ do
    it "produces valid DiffTime's" $ producesValidsOnValids timeAgo
    it "is the inverse of timeAgo for just picoseconds" $
      inverseFunctionsOnGen
        timeAgoToDiffTime
        timeAgo
        ((TimeAgo GT 0 0 0 0 <$> genValid) `suchThat` isValid)
        (const [])
    it "is the inverse of timeAgo" $ inverseFunctionsOnValid timeAgoToDiffTime timeAgo
  describe "renderDaysAgoAuto" $ do
    it "produces valid Strings's" $ producesValidsOnValids renderDaysAgoAuto
    let i da s = it (unwords ["Renders", show da, "as", show s]) $ renderDaysAgoAuto da `shouldBe` s
    describe "renders these simple examples well" $ do
      i (DaysAgo GT 0 0 0 5) "5 years ago"
      i (DaysAgo GT 0 0 4 0) "4 months ago"
      i (DaysAgo GT 0 3 0 0) "3 weeks ago"
      i (DaysAgo GT 2 0 0 0) "2 days ago"
      i (DaysAgo GT 1 0 0 0) "yesterday"
      i (DaysAgo EQ 0 0 0 0) "today"
      i (DaysAgo LT 1 0 0 0) "tomorrow"
      i (DaysAgo LT 2 0 0 0) "in 2 days"
      i (DaysAgo LT 0 3 0 0) "in 3 weeks"
      i (DaysAgo LT 0 0 4 0) "in 4 months"
      i (DaysAgo LT 0 0 0 5) "in 5 years"
    describe "handles singular nouns well" $ do
      i (DaysAgo GT 0 0 0 1) "1 year ago"
      i (DaysAgo GT 0 0 1 0) "1 month ago"
      i (DaysAgo GT 0 1 0 0) "1 week ago"
      i (DaysAgo GT 1 0 0 0) "yesterday"
      i (DaysAgo LT 1 0 0 0) "tomorrow"
      i (DaysAgo LT 0 1 0 0) "in 1 week"
      i (DaysAgo LT 0 0 1 0) "in 1 month"
      i (DaysAgo LT 0 0 0 1) "in 1 year"
  describe "renderTimeAgoAuto" $ do
    it "produces valid Strings's" $ producesValidsOnValids renderTimeAgoAuto
    let i ta s = it (unwords ["Renders", show ta, "as", show s]) $ renderTimeAgoAuto ta `shouldBe` s
    describe "renders these simple examples well" $ do
      i (TimeAgo GT 5 0 0 0 0) "5 days ago"
      i (TimeAgo GT 0 6 0 0 0) "6 hours ago"
      i (TimeAgo GT 0 0 7 0 0) "7 minutes ago"
      i (TimeAgo GT 0 0 0 8 0) "8 seconds ago"
      i (TimeAgo GT 0 0 0 0 9) "just now"
      i (TimeAgo EQ 0 0 0 0 0) "just now"
      i (TimeAgo LT 5 0 0 0 0) "in 5 days"
      i (TimeAgo LT 0 6 0 0 0) "in 6 hours"
      i (TimeAgo LT 0 0 7 0 0) "in 7 minutes"
      i (TimeAgo LT 0 0 0 8 0) "in 8 seconds"
      i (TimeAgo LT 0 0 0 0 9) "just now"
    describe "handles singular nouns well" $ do
      i (TimeAgo GT 1 0 0 0 0) "1 day ago"
      i (TimeAgo GT 0 1 0 0 0) "1 hour ago"
      i (TimeAgo GT 0 0 1 0 0) "1 minute ago"
      i (TimeAgo GT 0 0 0 1 0) "1 second ago"
      i (TimeAgo GT 0 0 0 0 1) "just now"
      i (TimeAgo EQ 0 0 0 0 0) "just now"
      i (TimeAgo LT 1 0 0 0 0) "in 1 day"
      i (TimeAgo LT 0 1 0 0 0) "in 1 hour"
      i (TimeAgo LT 0 0 1 0 0) "in 1 minute"
      i (TimeAgo LT 0 0 0 1 0) "in 1 second"
      i (TimeAgo LT 0 0 0 0 1) "just now"
