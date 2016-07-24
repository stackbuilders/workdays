----------------------------------------------------------------------
-- |
--
----------------------------------------------------------------------

module Workdays
  ( Date(..)
  , Year
  , Month
  , DayOfMonth
  , weekdays
  , workdays
  )
  where

-- containers
import Data.Set (Set)
import qualified Data.Set as Set

-- time
import qualified Data.Time as Time
import qualified Data.Time.Calendar.WeekDate as Time


-- $setup
-- >>> :set -XOverloadedLists


-- |
--
-- A year.

type Year = Integer


-- |
--
-- A month.

type Month = Int


-- |
--
-- A day of month.

type DayOfMonth = Int


-- |
--
-- A date.

data Date =
  Date
    { dateYear :: !Year
    , dateMonth :: {-# UNPACK #-} !Month
    , dateDay :: {-# UNPACK #-} !DayOfMonth
    }
  deriving (Eq, Ord, Show)


-- |
--
-- Calculate the number of weekdays between two dates.
--
-- >>> weekdays (Date 2016 01 04) (Date 2016 01 10)
-- 5
--
-- >>> weekdays (Date 2016 01 01) (Date 2016 12 31)
-- 261

weekdays
  :: Date -- ^ Start date
  -> Date -- ^ End date
  -> Integer
weekdays startDate endDate =
  workdays startDate endDate mempty


-- |
--
-- Calculate the number of workdays between two dates.
--
-- >>> workdays (Date 2016 01 01) (Date 2016 12 31) []
-- 261
--
-- >>> workdays (Date 2016 01 01) (Date 2016 12 31) [Date 2016 01 01]
-- 260

workdays
  :: Date -- ^ Start date
  -> Date -- ^ End date
  -> Set Date -- ^ Dates to exclude
  -> Integer

workdays startDate endDate holidays | startDate > endDate =
  negate (workdays endDate startDate holidays)

workdays startDate@(Date y1 m1 d1) endDate@(Date y2 m2 d2) holidays =
  let
    deltaDays =
      succ $
        Time.diffDays
          (Time.fromGregorian y2 m2 d2)
          (Time.fromGregorian y1 m1 d1)

    (fullWeeks, extraDays) = divMod deltaDays 7

    numWeekdays = fullWeeks * 5

    numWeekdays' = calc1 (fromIntegral extraDays :: Integer) numWeekdays

    sd x =
      Time.toGregorian $
        Time.addDays
          (x - 1)
          (Time.fromGregorian y1 m1 d1)

    calc1 0 w = w
    calc1 d w =
      let
        (y,m,d3) = sd (fromIntegral d)
      in
        if isWeekday (dayOfWeek (Date y m d3))
          then calc1 (d - 1) (w + 1)
          else calc1 (d - 1) w

    holidays' =
      Set.filter (\d -> isWeekday (dayOfWeek d) && inBetween d) holidays

    inBetween d = startDate <= d && d <= endDate
  in
    numWeekdays' - fromIntegral (length holidays')


dayOfWeek :: Date -> Int
dayOfWeek (Date y m d) =
  let (_,_,a) = Time.toWeekDate (Time.fromGregorian y m d) in a


isWeekday :: Int -> Bool
isWeekday = not . isWeekend


isWeekend :: Int -> Bool
isWeekend = flip elem ([6, 7] :: [Int])
