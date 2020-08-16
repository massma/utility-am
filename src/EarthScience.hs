{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

module EarthScience
  ( monthlyTOA,
    dailyTOALatBand,
    dailyTOA,
    instantTOA,

    -- * geometry tools
    latDegreeDistance,
    lonDegreeDistance,
    distanceFromDegreeLatLon,
    deltaLatFromKm,
    deltaLonFromKm,
    pressure,
    radiansFromDegrees,

    -- * water vapor and temperature tools
    vaporSatFromT,
    rhFromTDew,
    celsiusToF,
    fahrenheitToC,
    heatIndexFromTDew,
    heatIndex,
  )
where

import AMUtil
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.Time as DT
import qualified Geodetics.Geodetic as Geo
import qualified Numeric.Integration.TanhSinh as TS
import Numeric.Units.Dimensional
  ( Length,
    (*~),
    (/~),
  )
import qualified Numeric.Units.Dimensional.NonSI as NonSI
import qualified Numeric.Units.Dimensional.SIUnits as SI
import Prelude hiding
  ( head,
    init,
    last,
    read,
    tail,
    (!!),
  )

-- | from rose's climlab, flerchinger uses 1360
-- rose cites Trenberth and Fasullo, Surveys of Geophysics 2012
s0 :: Double
s0 = 1365.2

-- | calculates day angle:
-- FIXME: make getJulianDay fractional days, rather than stepwise
dayAngle :: Floating a => DT.UTCTime -> a
dayAngle t = pi * 2.0 * (realToFrac (getJulianDay t) - 1.0) / 365.0

sumCosSinSeries :: Floating a => [a] -> [a] -> a -> a
sumCosSinSeries an bn theta =
  L.sum $
    (\(a, b, n) -> a * cos (n * theta) + b * sin (n * theta))
      <$> L.zip3
        an
        bn
        (fmap realToFrac [0 .. (length an - 1)])

declination :: Floating a => DT.UTCTime -> a
declination time =
  let an = [0.006918, -0.399912, -0.006758, -0.002697]
      bn = [0.0, 0.070257, 0.000907, 0.001480]
      theta = dayAngle time
   in sumCosSinSeries an bn theta

dbarDsquared :: Floating a => DT.UTCTime -> a
dbarDsquared time =
  let an = [1.000110, 0.034221, 0.000719]
      bn = [0.0, 0.001280, 0.000077]
      theta = dayAngle time
   in sumCosSinSeries an bn theta

hourAngle :: Floating a => a -> DT.UTCTime -> a
hourAngle lon time =
  let gmtNoon = DT.UTCTime (DT.utctDay time) (12 * 60 * 60)
      hourOffset = diffHours time gmtNoon
   in 2.0 * pi * hourOffset / 24.0 + lon

-- important! time is GMT
-- FIXME : think I'm missing some fancy solar terms here, straight lon not good

instantTOA ::
  -- | lat (radians)
  Double ->
  -- | lon (radians)
  Double ->
  -- | time
  DT.UTCTime ->
  Double
instantTOA lat lon time =
  let dec = declination time
   in max
        ( s0
            * dbarDsquared time
            * (sin lat * sin dec + cos lat * cos dec * cos (hourAngle lon time))
        )
        0.0

h0 ::
  -- | lat (radians)
  Double ->
  -- | declination (radians)
  Double ->
  -- | h0 (radians)
  Double
h0 lat dec
  | x > 1.0 = 0.0
  | x < (-1.0) = pi
  | otherwise = acos x
  where
    x = - tan lat * tan dec

dailyTOA ::
  -- | lat (radians)
  Double ->
  -- | lon (radians)
  Double ->
  -- | time
  DT.UTCTime ->
  Double
dailyTOA lat _lon day =
  let tmid = addHours 12 day
      d = declination tmid
      h = h0 lat d
   in -- max
      --   0.0
      ( s0
          / pi
          * dbarDsquared tmid
          * (h * sin lat * sin d + cos lat * cos d * sin h)
      )

dailyTOALatBand ::
  -- | min lat
  Double ->
  -- | max lat
  Double ->
  -- | day
  DT.UTCTime ->
  -- | mean radiation
  Double
dailyTOALatBand latmin latmax day =
  TS.result
    ( TS.absolute 1.0e-5 $
        TS.trap (\lat -> cos lat * dailyTOA lat 0.0 day) latmin latmax
    )
    / (sin latmax - sin latmin)

monthlyTOA ::
  -- | lat (radians)
  Double ->
  -- | lon (radians)
  Double ->
  -- | time
  DT.UTCTime ->
  Double
monthlyTOA lat lon time =
  fromMaybe
    ( error
        "this should be impossible, list should never be empty if we add one month"
    )
    $ averageBy (dailyTOA lat lon) $
      L.takeWhile (< addMonths 1 time) $
        L.iterate (addDays 1) time

-- hour angle is correct
-- (hourAngle 0.0 (utcTimeConstructor 2012 2 1 13 0 0)) = 0.2617993877991494
-- (instantToa (radiansFromDegrees 45.0) 0.0 (utcTimeConstructor 2012 2 1 13 0 0)) = 620.915926851355
-- (monthlyToa (radiansFromDegrees 45.0) 0.0 (utcTimeConstructor 2012 2 1 0 0 0)) = 205.39394152650758
-- Geometry tools

earthRadius :: Length Double
earthRadius = 6378 *~ (SI.kilo SI.meter)

eccentricitySquared :: Double
eccentricitySquared = 0.00669437999014

radiansFromDegrees :: Floating a => a -> a
radiansFromDegrees deg = deg / 180.0 * pi

buildGeo :: Double -> Double -> Geo.Geodetic Geo.WGS84
buildGeo lat lon =
  Geo.Geodetic (lat *~ SI.degree) (lon *~ SI.degree) (0.0 *~ SI.meter) Geo.WGS84

-- | returns the distance between two points in km, do we want to leave in dimensional?
distanceFromDegreeLatLon ::
  -- | first lat, lon
  (Double, Double) ->
  -- | second lat, lon
  (Double, Double) ->
  -- | km
  Double
distanceFromDegreeLatLon (lat1, lon1) (lat2, lon2) =
  case Geo.groundDistance (buildGeo lat1 lon1) (buildGeo lat2 lon2) of
    Nothing -> pi * (earthRadius /~ (SI.kilo SI.meter)) -- accorgin to the docs, this only fails when antipodal (half circumference away)
    Just (distance, _, _) -> distance /~ (SI.kilo SI.meter)

-- distance between
-- (40.810779, -73.958897)
-- and
-- 40.730816, -73.997458
-- 9.46 km

-- | returns distance in km of one degree latitude from latitude in
-- degrees
latDegreeDistance :: Double -> Double
latDegreeDistance lat =
  distanceFromDegreeLatLon (lat - 0.5, 0.0) (lat + 0.5, 0.0)

-- >>> latDegreeDistance 47.0
-- 111.17083975591227

-- | returns distance in km of one degree longitude from latitude in
-- degrees
lonDegreeDistance :: Double -> Double
lonDegreeDistance lat = distanceFromDegreeLatLon (lat, 0.0) (lat, 1.0)

-- >>> lonDegreeDistance 47.0
-- 76.0554819045587

-- | returns distance in lat degrees from lon center and km distance
-- degrees WGS84 spheroid, see https://en.wikipedia.onrg/wiki/Latitude
deltaLatFromKm :: Double -> Double -> Double
deltaLatFromKm km latCenter = km / latDegreeDistance latCenter

-- | returns distance in lon degrees from lat center and km distance
-- WGS84 spheroid, see https://en.wikipedia.org/wiki/Latitude
deltaLonFromKm :: Double -> Double -> Double
deltaLonFromKm km latCenter = km / lonDegreeDistance latCenter

pressure :: Floating a => a -> a
pressure z =
  let p0 = 100000.0
      h = 8000.0
   in p0 * exp (- z / h)

-- H is same units as z, and p0 is same as output

-- | equation 2.17 in Shuttleworth
vaporSatFromT ::
  -- | temperature in celsius
  Double ->
  -- | saturation vapor pressure in Pa
  Double
vaporSatFromT t = 610.8 * exp (17.27 * t / (237.3 + t))

-- | equation 2.19 in Shuttleworth
rhFromTDew ::
  -- | temperature (celsius)
  Double ->
  -- | dew point temperature (celsius)
  Double ->
  -- | rh in percent
  Double
rhFromTDew t tdew = 100 * vaporSatFromT tdew / vaporSatFromT t

celsiusToF :: Double -> Double
celsiusToF t = ((t *~ SI.degreeCelsius) /~ NonSI.degreeFahrenheit) + 32.0

fahrenheitToC :: Double -> Double
fahrenheitToC t = (((t - 32) *~ NonSI.degreeFahrenheit) /~ SI.degreeCelsius)

-- | from Schoen 2005, but note after playing around with it seems to
-- deviate from NWS at high values. not sure which is actually correct
-- (NWS says on website only valid for range of values in table), but
-- for consistancy recommend useing NWS, for most of my applications I
-- am in the range of values on the table anwways.  NOTE: it does
-- appear that Shoen deviatse from values in the table as well
-- (e.g. T: 28 C Tdew: 28 C) Not sure what this means for the paper...
-- But think should def. use NWS formula instead. Weird that Schoen
-- didn't acknowledge this or draw attention to it more in the paper
heatIndexFromTDewSchoen ::
  -- | temperature (celsius)
  Double ->
  -- | dew point temperature (celsius)
  Double ->
  -- | heat index in Fahrenheit
  Double
heatIndexFromTDewSchoen t tdew =
  celsiusToF (t - 1.0799 * exp (0.03755 * t) * (1 - exp (0.0801 * (tdew - 14))))

-- | see https://www.wpc.ncep.noaa.gov/html/heatindex_equation.shtml
heatIndexFromTDew ::
  -- | temperature (celsius)
  Double ->
  -- | dew point temperature (celsius)
  Double ->
  -- | heat index in Fahrenheit
  Double
heatIndexFromTDew t tdew = heatIndex tf rh
  where
    tf = celsiusToF t
    rh = rhFromTDew t tdew

heatIndex ::
  -- | temperature (fahrenheit)
  Double ->
  -- | rh (percent)
  Double ->
  -- | heat index (fahrenheit)
  Double
heatIndex tf rh = if (0.5 * (hi1 + tf)) > 80.0 then fullRegression else hi1
  where
    hi1 = 0.5 * (tf + 61.0 + ((tf - 68.0) * 1.2) + (rh * 0.094))
    adjustment =
      if (rh < 13) && (tf >= 80) && (tf <= 112)
        then negate (((13 - rh) / 4) * sqrt ((17 - abs (tf - 95)) / 17))
        else
          ( if (rh > 85) && (tf >= 80) && (tf <= 87)
              then ((rh - 85) / 10) * ((87 - tf) / 5)
              else 0.0
          )
    fullRegression =
      -42.379
        + 2.04901523
        * tf
        + 10.14333127
        * rh
        - 0.22475541
        * tf
        * rh
        - 0.00683783
        * tf
        * tf
        - 0.05481717
        * rh
        * rh
        + 0.00122874
        * tf
        * tf
        * rh
        + 0.00085282
        * tf
        * rh
        * rh
        - 0.00000199
        * tf
        * tf
        * rh
        * rh
        + adjustment

-- heatIndexFromTDew 30 0 == 82.64377661302215
-- heatIndexFromTDew 30 30 == 111.84952409999984
-- heatIndexFromTDew 30 20 == 89.43262298840472
