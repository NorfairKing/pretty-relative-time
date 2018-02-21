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

instance GenUnchecked TimeAgo

instance GenValid TimeAgo where
    genValid = do
        sign <- genValid
        case sign of
            EQ -> pure $ TimeAgo EQ 0 0 0 0 0
            _ ->
                (TimeAgo sign <$> choose (0, picoSecondsPerSecond) <*>
                 choose (0, secondsPerMinute) <*>
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
        it "is the inverse of timeAgoToDiffTime" $
            inverseFunctionsOnValid timeAgo timeAgoToDiffTime
    describe "timeAgoToDiffTime" $ do
        it "produces valid DiffTime's" $ producesValidsOnValids timeAgo
        it "is the inverse of timeAgo for just picoseconds" $
            inverseFunctionsOnGen
                timeAgoToDiffTime
                timeAgo
                ((TimeAgo GT 0 0 0 0 <$> genValid) `suchThat` isValid)
                (const [])
        it "is the inverse of timeAgo" $
            inverseFunctionsOnValid timeAgoToDiffTime timeAgo
    describe "renderTimeAgoAuto" $ do
        it "produces valid Strings's" $ producesValidsOnValids renderTimeAgoAuto
        it "renders these simple examples well" $ do
            renderTimeAgoAuto (TimeAgo GT 5 0 0 0 0) `shouldBe` "5 days ago"
            renderTimeAgoAuto (TimeAgo GT 0 6 0 0 0) `shouldBe` "6 hours ago"
            renderTimeAgoAuto (TimeAgo GT 0 0 7 0 0) `shouldBe` "7 minutes ago"
            renderTimeAgoAuto (TimeAgo GT 0 0 0 8 0) `shouldBe` "8 seconds ago"
            renderTimeAgoAuto (TimeAgo GT 0 0 0 0 9) `shouldBe` "just now"
            renderTimeAgoAuto (TimeAgo EQ 0 0 0 0 0) `shouldBe` "just now"
            renderTimeAgoAuto (TimeAgo LT 5 0 0 0 0) `shouldBe` "in 5 days"
            renderTimeAgoAuto (TimeAgo LT 0 6 0 0 0) `shouldBe` "in 6 hours"
            renderTimeAgoAuto (TimeAgo LT 0 0 7 0 0) `shouldBe` "in 7 minutes"
            renderTimeAgoAuto (TimeAgo LT 0 0 0 8 0) `shouldBe` "in 8 seconds"
            renderTimeAgoAuto (TimeAgo LT 0 0 0 0 9) `shouldBe` "just now"
