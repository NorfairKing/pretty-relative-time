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
