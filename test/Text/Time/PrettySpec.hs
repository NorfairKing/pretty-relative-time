{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Time.PrettySpec
  ( spec
  ) where

import Debug.Trace

import Numeric.Natural

import Data.GenValidity.Time ()
import Test.Hspec
import Test.Hspec.QuickCheck
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
        (DaysAgo sign <$> (natUpTo 364) <*> (natUpTo 29) <*> (natUpTo 4) <*> (natUpTo 7)) `suchThat`
        isValid
  shrinkValid = shrinkValidStructurally

instance GenUnchecked TimeAgo

instance GenValid TimeAgo where
  genValid = do
    sign <- genValid
    case sign of
      EQ -> pure $ TimeAgo EQ (DaysAgo EQ 0 0 0 0) 0 0 0 0
      _ ->
        TimeAgo sign <$> (genValid `suchThat` ((/= LT) . daysAgoSign)) <*> natUpTo 23 <*> natUpTo 59 <*>
        natUpTo 59 <*>
        natUpTo 5
  shrinkValid (TimeAgo o da h m s ps) =
    filter
      isValid
      [TimeAgo o' da' h' m' s' ps | (o', da', h', m', s') <- shrinkValid (o, da, h, m, s)]

natUpTo :: Integer -> Gen Natural
natUpTo = chooseNat 0

chooseNat :: Integer -> Integer -> Gen Natural
chooseNat lo hi = fromInteger <$> choose (lo, hi)

spec :: Spec
spec = do
  eqSpec @DaysAgo
  genValidSpec @DaysAgo
  describe "daysAgo" $ do
    it "produces valid TimeAgos" $ producesValidsOnValids daysAgo
    it "is the inverse of daysAgoToDays" $ inverseFunctionsOnValid daysAgo daysAgoToDays
  describe "daysAgoToDays" $ do
    it "produces valid results" $ producesValidsOnValids daysAgoToDays
    it "is the inverse of daysAgo for just days" $
      inverseFunctionsOnGen
        daysAgoToDays
        daysAgo
        (((\d -> DaysAgo GT 0 d 0 0) <$> genValid) `suchThat` isValid)
        (const [])
    it "is the inverse of daysAgo" $ inverseFunctionsOnValid daysAgoToDays daysAgo
  describe "renderDaysAgoAuto" $ do
    it "produces valid Strings" $ producesValidsOnValids renderDaysAgoAuto
    let i da s = it (unwords ["Renders", show da, "as", show s]) $ renderDaysAgoAuto da `shouldBe` s
    describe "renders these simple examples well" $ do
      i (DaysAgo GT 5 0 0 0) "5 years ago"
      i (DaysAgo GT 0 4 0 0) "4 months ago"
      i (DaysAgo GT 0 0 3 0) "3 weeks ago"
      i (DaysAgo GT 0 0 0 2) "2 days ago"
      i (DaysAgo GT 0 0 0 1) "yesterday"
      i (DaysAgo EQ 0 0 0 0) "today"
      i (DaysAgo LT 0 0 0 1) "tomorrow"
      i (DaysAgo LT 0 0 0 2) "in 2 days"
      i (DaysAgo LT 0 0 3 0) "in 3 weeks"
      i (DaysAgo LT 0 4 0 0) "in 4 months"
      i (DaysAgo LT 5 0 0 0) "in 5 years"
    describe "handles singular nouns well" $ do
      i (DaysAgo GT 1 0 0 0) "1 year ago"
      i (DaysAgo GT 0 1 0 0) "1 month ago"
      i (DaysAgo GT 0 0 1 0) "1 week ago"
      i (DaysAgo GT 0 0 0 1) "yesterday"
      i (DaysAgo LT 0 0 0 1) "tomorrow"
      i (DaysAgo LT 0 0 1 0) "in 1 week"
      i (DaysAgo LT 0 1 0 0) "in 1 month"
      i (DaysAgo LT 1 0 0 0) "in 1 year"
  eqSpec @TimeAgo
  genValidSpec @TimeAgo
  describe "timeAgo" $ do
    it "produces valid TimeAgo's" $ producesValidsOnValids timeAgo
    it "is the inverse of timeAgoToDiffTime" $ inverseFunctionsOnValid timeAgo timeAgoToDiffTime
  describe "timeAgoToDiffTime" $ do
    it "produces valid DiffTime's" $ producesValidsOnValids (timeAgoToDiffTime . traceShowId)
    -- it "is the inverse of timeAgo for just picoseconds" $
    --   inverseFunctionsOnGen
    --     timeAgoToDiffTime
    --     timeAgo
    --     ((TimeAgo GT (DaysAgo EQ 0 0 0 0) 0 0 0 <$> natUpTo (1 ^ 12)) `suchThat` isValid)
    --     (const [])
    it "is the inverse of timeAgo" $ inverseFunctionsOnValid timeAgoToDiffTime timeAgo
  describe "renderTimeAgoAuto" $ do
    it "produces valid Strings's" $ producesValidsOnValids renderTimeAgoAuto
    let i ta s = it (unwords ["Renders", show ta, "as", show s]) $ renderTimeAgoAuto ta `shouldBe` s
    describe "renders these simple examples well" $ do
      i (TimeAgo GT (DaysAgo GT 2 0 0 0) 0 0 0 0) "2 years ago"
      i (TimeAgo GT (DaysAgo GT 0 3 0 0) 0 0 0 0) "3 months ago"
      i (TimeAgo GT (DaysAgo GT 0 0 4 0) 0 0 0 0) "4 weeks ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 5) 0 0 0 0) "5 days ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 0) 6 0 0 0) "6 hours ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 0) 0 7 0 0) "7 minutes ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 0) 0 0 8 0) "8 seconds ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 0) 0 0 0 9) "just now"
      i (TimeAgo EQ (DaysAgo EQ 0 0 0 0) 0 0 0 0) "just now"
      i (TimeAgo LT (DaysAgo GT 2 0 0 0) 0 0 0 0) "in 2 years"
      i (TimeAgo LT (DaysAgo GT 0 3 0 0) 0 0 0 0) "in 3 months"
      i (TimeAgo LT (DaysAgo GT 0 0 4 0) 0 0 0 0) "in 4 weeks"
      i (TimeAgo LT (DaysAgo GT 0 0 0 5) 0 0 0 0) "in 5 days"
      i (TimeAgo LT (DaysAgo GT 0 0 0 0) 6 0 0 0) "in 6 hours"
      i (TimeAgo LT (DaysAgo GT 0 0 0 0) 0 7 0 0) "in 7 minutes"
      i (TimeAgo LT (DaysAgo GT 0 0 0 0) 0 0 8 0) "in 8 seconds"
      i (TimeAgo LT (DaysAgo GT 0 0 0 0) 0 0 0 9) "just now"
    describe "handles singular nouns well" $ do
      i (TimeAgo GT (DaysAgo GT 1 0 0 0) 0 0 0 0) "1 year ago"
      i (TimeAgo GT (DaysAgo GT 0 1 0 0) 0 0 0 0) "1 month ago"
      i (TimeAgo GT (DaysAgo GT 0 0 1 0) 0 0 0 0) "1 week ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 1) 0 0 0 0) "1 day ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 0) 1 0 0 0) "1 hour ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 0) 0 1 0 0) "1 minute ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 0) 0 0 1 0) "1 second ago"
      i (TimeAgo GT (DaysAgo GT 0 0 0 0) 0 0 0 1) "just now"
      i (TimeAgo EQ (DaysAgo EQ 0 0 0 0) 0 0 0 0) "just now"
      i (TimeAgo LT (DaysAgo GT 1 0 0 0) 0 0 0 0) "in 1 year"
      i (TimeAgo LT (DaysAgo GT 0 1 0 0) 0 0 0 0) "in 1 month"
      i (TimeAgo LT (DaysAgo GT 0 0 1 0) 0 0 0 0) "in 1 week"
      i (TimeAgo LT (DaysAgo GT 0 0 0 1) 0 0 0 0) "in 1 day"
      i (TimeAgo LT (DaysAgo GT 0 0 0 0) 1 0 0 0) "in 1 hour"
      i (TimeAgo LT (DaysAgo GT 0 0 0 0) 0 1 0 0) "in 1 minute"
      i (TimeAgo LT (DaysAgo GT 0 0 0 0) 0 0 1 0) "in 1 second"
      i (TimeAgo LT (DaysAgo GT 0 0 0 0) 0 0 0 1) "just now"
