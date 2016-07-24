{-# LANGUAGE OverloadedLists #-}

module WorkdaysSpec
  ( main
  , spec
  )
  where

-- workdays
import Workdays

-- containers
import Data.Set (Set)

-- hspec
import Test.Hspec


main :: IO ()
main =
  hspec spec


spec :: Spec
spec =
  describe "" $ do
    it "some workdays with no holidays" $
      workdays (Date 2015 08 01) (Date 2015 09 30) [] `shouldBe` 43

    it "some workdays with a holiday" $
      workdays (Date 2015 08 01) (Date 2015 09 30) [Date 2015 09 07] `shouldBe` 42

    it "workdays 2016" $
      workdays (Date 2016 01 01) (Date 2016 12 31) holidays `shouldBe` 250

    it "weekdays 2010" $
      workdays (Date 2010 01 01) (Date 2010 12 31) [] `shouldBe` 261
    it "weekdays 2011" $
      workdays (Date 2011 01 01) (Date 2011 12 31) [] `shouldBe` 260
    it "weekdays 2012" $
      workdays (Date 2012 01 01) (Date 2012 12 31) [] `shouldBe` 261
    it "weekdays 2013" $
      workdays (Date 2013 01 01) (Date 2013 12 31) [] `shouldBe` 261
    it "weekdays 2014" $
      workdays (Date 2014 01 01) (Date 2014 12 31) [] `shouldBe` 261
    it "weekdays 2015" $
      workdays (Date 2015 01 01) (Date 2015 12 31) [] `shouldBe` 261
    it "weekdays 2016" $
      workdays (Date 2016 01 01) (Date 2016 12 31) [] `shouldBe` 261
    it "weekdays 2017" $
      workdays (Date 2017 01 01) (Date 2017 12 31) [] `shouldBe` 260
    it "weekdays 2018" $
      workdays (Date 2018 01 01) (Date 2018 12 31) [] `shouldBe` 261
    it "weekdays 2019" $
      workdays (Date 2019 01 01) (Date 2019 12 31) [] `shouldBe` 261
    it "weekdays 2020" $
      workdays (Date 2020 01 01) (Date 2020 12 31) [] `shouldBe` 262

    it "weekdays 2010--2020" $
      workdays (Date 2010 01 01) (Date 2020 12 31) [] `shouldBe` 2870


holidays :: Set Date
holidays =
  [ Date 2016 01 01 -- New Year's Day (Friday)
  , Date 2016 01 18 -- Martin Luther King Day (Monday)
  , Date 2016 02 14 -- Valentine's Day (Sunday)
  , Date 2016 02 15 -- Presidents' Day (Monday)
  , Date 2016 03 27 -- Easter Sunday (Sunday)
  , Date 2016 04 13 -- Thomas Jefferson's birthday (Wednesday)
  , Date 2016 05 08 -- Mother's Day (Sunday)
  , Date 2016 05 30 -- Memorial Day (Monday)
  , Date 2016 06 19 -- Father's Day (Sunday)
  , Date 2016 07 04 -- Independence Day (Monday)
  , Date 2016 09 05 -- Labor Day (Monday)
  , Date 2016 10 31 -- Halloween (Monday)
  , Date 2016 11 11 -- Veterans Day (Friday)
  , Date 2016 11 24 -- Thanksgiving Day
  , Date 2016 12 24 -- Christmas Eve (Saturday)
  , Date 2016 12 25 -- Christmas Day (Sunday)
  , Date 2016 12 26 -- Christmas Day observed (Monday)
  , Date 2016 12 31 -- New Year's Eve (Saturday)
  ]
