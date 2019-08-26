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
      EQ -> pure $ TimeAgo EQ 0 0 0 0 0 0
      _ ->
        (TimeAgo sign <$> (abs <$> genValid) <*> choose (0, daysPerWeek) <*> choose (0, hoursPerDay) <*>
         choose (0, minutesPerHour) <*>
         choose (0, secondsPerMinute) <*>
         choose (0, picoSecondsPerSecond)) `suchThat`
        isValid

spec :: Spec
spec = do
  eqSpec @DaysAgo
  genValidSpec @DaysAgo
  describe "daysAgo" $ do
    it "produces valid TimeAgo's" $ producesValidsOnValids daysAgo
    it "is the inverse of daysAgoToDays" $ inverseFunctionsOnValid daysAgo daysAgoToDays
  describe "daysAgoToDays" $ do
    it "produces valid DiffTime's" $ producesValidsOnValids daysAgo
    it "is the inverse of daysAgo for just days" $
      inverseFunctionsOnGen
        daysAgoToDays
        daysAgo
        (((\d -> DaysAgo GT 0 d 0 0) <$> genValid) `suchThat` isValid)
        (const [])
    it "is the inverse of daysAgo" $ inverseFunctionsOnValid daysAgoToDays daysAgo
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
        ((TimeAgo GT 0 0 0 0 0 <$> genValid) `suchThat` isValid)
        (const [])
    it "is the inverse of timeAgo" $ inverseFunctionsOnValid timeAgoToDiffTime timeAgo
  describe "renderTimeAgoAuto" $ do
    it "produces valid Strings's" $ producesValidsOnValids renderTimeAgoAuto
    it "renders these simple examples well" $ do
      renderTimeAgoAuto (TimeAgo GT 4 0 0 0 0 0) `shouldBe` "4 weeks ago"
      renderTimeAgoAuto (TimeAgo GT 0 5 0 0 0 0) `shouldBe` "5 days ago"
      renderTimeAgoAuto (TimeAgo GT 0 0 6 0 0 0) `shouldBe` "6 hours ago"
      renderTimeAgoAuto (TimeAgo GT 0 0 0 7 0 0) `shouldBe` "7 minutes ago"
      renderTimeAgoAuto (TimeAgo GT 0 0 0 0 8 0) `shouldBe` "8 seconds ago"
      renderTimeAgoAuto (TimeAgo GT 0 0 0 0 0 9) `shouldBe` "just now"
      renderTimeAgoAuto (TimeAgo EQ 0 0 0 0 0 0) `shouldBe` "just now"
      renderTimeAgoAuto (TimeAgo LT 4 0 0 0 0 0) `shouldBe` "in 4 weeks"
      renderTimeAgoAuto (TimeAgo LT 0 5 0 0 0 0) `shouldBe` "in 5 days"
      renderTimeAgoAuto (TimeAgo LT 0 0 6 0 0 0) `shouldBe` "in 6 hours"
      renderTimeAgoAuto (TimeAgo LT 0 0 0 7 0 0) `shouldBe` "in 7 minutes"
      renderTimeAgoAuto (TimeAgo LT 0 0 0 0 8 0) `shouldBe` "in 8 seconds"
      renderTimeAgoAuto (TimeAgo LT 0 0 0 0 0 9) `shouldBe` "just now"
    it "handles singular nouns well" $ do
      renderTimeAgoAuto (TimeAgo GT 1 0 0 0 0 0) `shouldBe` "1 week ago"
      renderTimeAgoAuto (TimeAgo GT 0 1 0 0 0 0) `shouldBe` "1 day ago"
      renderTimeAgoAuto (TimeAgo GT 0 0 1 0 0 0) `shouldBe` "1 hour ago"
      renderTimeAgoAuto (TimeAgo GT 0 0 0 1 0 0) `shouldBe` "1 minute ago"
      renderTimeAgoAuto (TimeAgo GT 0 0 0 0 1 0) `shouldBe` "1 second ago"
      renderTimeAgoAuto (TimeAgo GT 0 0 0 0 0 1) `shouldBe` "just now"
      renderTimeAgoAuto (TimeAgo EQ 0 0 0 0 0 0) `shouldBe` "just now"
      renderTimeAgoAuto (TimeAgo LT 1 0 0 0 0 0) `shouldBe` "in 1 week"
      renderTimeAgoAuto (TimeAgo LT 0 1 0 0 0 0) `shouldBe` "in 1 day"
      renderTimeAgoAuto (TimeAgo LT 0 0 1 0 0 0) `shouldBe` "in 1 hour"
      renderTimeAgoAuto (TimeAgo LT 0 0 0 1 0 0) `shouldBe` "in 1 minute"
      renderTimeAgoAuto (TimeAgo LT 0 0 0 0 1 0) `shouldBe` "in 1 second"
      renderTimeAgoAuto (TimeAgo LT 0 0 0 0 0 1) `shouldBe` "just now"
      renderTimeAgoAuto (TimeAgo LT 0 0 0 0 0 1) `shouldBe` "just now"
