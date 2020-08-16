{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module AMUtil
  ( addMonths,
    addDays,
    addHours,
    avg,
    average,
    averageBy,
    averageVariance,
    averageStd,
    diffHours,
    diffMinutes,
    diffSeconds,
    doubleOfText,
    fsharpSlice,
    getJulianDay,
    getYear,
    getMonth,
    getDay,
    getHour,
    getMinute,
    intOfText,
    integerOfText,
    schemeSlice,
    utcTimeConstructor,
    yearMonthDayToUtc,
    yearToUtc,
    variance,
    varianceGeneric,
    groupByToMap,
    groupByAccessor,
    foldM',
    arity2Compose,
    utcToLocal,
    localToUtc,
  )
where

import Data.Foldable (foldl')
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import qualified Data.Time as DT
import Text.Read (readMaybe)
import Prelude hiding
  ( head,
    init,
    last,
    maximum,
    minimum,
    read,
    tail,
    (!!),
  )

yearMonthDayToUtc :: Integer -> Int -> Int -> DT.UTCTime
yearMonthDayToUtc year month day =
  DT.UTCTime (DT.fromGregorian year month day) 0

yearToUtc :: Integer -> DT.UTCTime
yearToUtc year = yearMonthDayToUtc year 1 1

utcTimeConstructor :: Integer -> Int -> Int -> Int -> Int -> Int -> DT.UTCTime
utcTimeConstructor year month day hour minute sec =
  DT.UTCTime
    (DT.fromGregorian year month day)
    ( DT.secondsToDiffTime
        (fromIntegral hour * 60 * 60 + fromIntegral minute * 60 + fromIntegral sec)
    )

getYear :: (DT.UTCTime -> Int)
getYear t =
  let (year, _month, _day) = DT.toGregorian (DT.utctDay t) in fromInteger year

getMonth :: (DT.UTCTime -> Int)
getMonth t = let (_year, month, _day) = DT.toGregorian (DT.utctDay t) in month

getDay :: (DT.UTCTime -> Int)
getDay t = let (_year, _month, day) = DT.toGregorian (DT.utctDay t) in day

getHour :: (DT.UTCTime -> Int)
getHour t = DT.todHour (DT.timeToTimeOfDay (DT.utctDayTime t))

getMinute :: (DT.UTCTime -> Int)
getMinute t = DT.todMin (DT.timeToTimeOfDay (DT.utctDayTime t))

getJulianDay :: DT.UTCTime -> Int
getJulianDay t = fromIntegral days + 1
  where
    days =
      DT.diffDays (DT.utctDay t) (DT.fromGregorian (fromIntegral (getYear t)) 1 1)

diffSeconds :: Fractional b => DT.UTCTime -> DT.UTCTime -> b
diffSeconds t1 t2 = realToFrac $ DT.diffUTCTime t1 t2

diffMinutes :: Fractional a => DT.UTCTime -> DT.UTCTime -> a
diffMinutes t1 t2 = (/ 60) $ diffSeconds t1 t2

diffHours :: Fractional a => DT.UTCTime -> DT.UTCTime -> a
diffHours t1 t2 = (/ (60 * 60)) $ diffSeconds t1 t2

addMonths :: Integer -> DT.UTCTime -> DT.UTCTime
addMonths months t =
  DT.UTCTime
    (DT.addGregorianMonthsClip months (DT.utctDay t))
    (DT.utctDayTime t)

addDays :: Integer -> DT.UTCTime -> DT.UTCTime
addDays days t = DT.UTCTime (DT.addDays days (DT.utctDay t)) (DT.utctDayTime t)

addHours :: Int -> DT.UTCTime -> DT.UTCTime
addHours hours = DT.addUTCTime (fromIntegral (hours * 60 * 60))

avg :: Fractional a => a -> a -> a
avg x y = 0.5 * (x + y)

average :: (Eq (t a), Monoid (t a), Fractional a, Foldable t) => t a -> Maybe a
average xs
  | xs == mempty = Nothing
  | otherwise = Just $ summed / fromIntegral (len :: Int)
  where
    (summed, len) = foldl' (\(sum', len') x -> (sum' + x, len' + 1)) (0, 0) xs

-- | wrong for samples, will underestimate variance
variance ::
  (Eq (t b), Monoid (t b), Fractional b, Foldable t, Functor t) =>
  t b ->
  Maybe b
variance xs =
  average xs >>= \xbar -> average $ fmap (\x -> (x - xbar) ^ (2 :: Int)) xs

varianceGeneric ::
  (Eq (t b), Monoid (t b), Fractional b, Foldable t, Functor t) =>
  t b ->
  Maybe b
varianceGeneric xs =
  average xs >>= \xbar -> average $ fmap (\x -> (x - xbar) ^ (2 :: Int)) xs

averageVariance ::
  (Eq (t1 t2), Monoid (t1 t2), Fractional t2, Foldable t1, Functor t1) =>
  t1 t2 ->
  Maybe (t2, t2)
averageVariance xs =
  average xs >>= \xbar ->
    (xbar,) <$> (average $ fmap (\x -> (x - xbar) ^ (2 :: Int)) xs)

-- | wrong for samples, will underestimate variance
averageStd ::
  (Eq (t1 t2), Monoid (t1 t2), Foldable t1, Floating t2, Functor t1) =>
  t1 t2 ->
  Maybe (t2, t2)
averageStd xs =
  average xs >>= \xbar ->
    ((xbar,) . sqrt) <$> (average $ fmap (\x -> (x - xbar) ** 2) xs)

averageBy ::
  (Eq (t a1), Monoid (t a1), Fractional a1, Foldable t, Functor t) =>
  (a2 -> a1) ->
  t a2 ->
  Maybe a1
averageBy f = (average . fmap f)

fsharpSlice :: Int -> Int -> Text.Text -> Text.Text
fsharpSlice start end line = Text.take (end - start + 1) (Text.drop start line)

schemeSlice :: Int -> Int -> Text.Text -> Text.Text
schemeSlice start end line = Text.take (end - start) (Text.drop start line)

doubleOfText :: Text.Text -> Maybe Double
doubleOfText = readMaybe . Text.unpack . Text.filter (/= '+')

integerOfText :: Text.Text -> Integer
integerOfText str =
  fromMaybe (-9999) (readMaybe (Text.unpack str) :: Maybe Integer)

intOfText :: Text.Text -> Int
intOfText str = fromMaybe (-9999) (readMaybe (Text.unpack str) :: Maybe Int)

-- | note below is memory intesive, also sequence will be faster appends
-- should probably do sort and then groupBy - or jsut get reid of these funcs
-- they were only around fromw hen I was learning and trasitioning from F#
groupByToMap :: Ord k => (a -> k) -> [a] -> Map.Map k [a]
groupByToMap accessor xs =
  Map.fromListWith (++) $ fmap (\x -> (accessor x, [x])) xs

-- | note below is just for old compatability
-- for data-clean-exe. should really do groupby . sort
groupByAccessor :: Ord k => (a -> k) -> [a] -> [(k, [a])]
groupByAccessor accessor = Map.toList . groupByToMap accessor

-- groupByAccessor f xs =
--   fmap (\(x:xs') -> ((f x), (x:xs'))) $
--   L.groupBy (\x0 x1 -> f x0 == f x1) xs
foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' _ z [] = return z
foldM' f z (x : xs) = do
  z' <- f z x
  z' `seq` foldM' f z' xs

arity2Compose :: (t1 -> t2 -> t3) -> (t4 -> t1) -> (t5 -> t2) -> t4 -> t5 -> t3
arity2Compose f fstf secondf x y = f (fstf x) (secondf y)

hourOffset :: Double -> Double
hourOffset lon = lon / 360.0 * 24.0

utcToLocal ::
  -- |  degrees!
  Double ->
  Int ->
  Double
utcToLocal lon t
  | t' < 0 = t' + 24
  | t' >= 24 = t' - 24
  | otherwise = t'
  where
    t' = (fromIntegral t) + hourOffset lon

localToUtc :: Double -> Double -> Int
localToUtc lon t
  | t' < 0 = t' + 24
  | t' >= 24 = t' - 24
  | otherwise = t'
  where
    t' = round (t - hourOffset lon)
