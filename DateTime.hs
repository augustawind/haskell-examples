import Data.List (elemIndex)


data DateTime = DateTime { getDate :: Date
                         , getTime :: Time
                         } deriving (Show, Read, Eq, Ord)

-- ---------------------------------------------------------------------------

data Date = Date { getYear :: Year
                 , getMonth :: Month
                 , getDay :: Day
                 } deriving (Show, Read, Eq, Ord)

datePlus :: Date -> Date -> Date
(Date y m d) `datePlus` (Date y' m' d') = Date (yy) (mm) (dd)
    where yy = addYears y y'
          mm = addMonths m m'
          dd = addDays d d'

-- ---------------------------------------------------------------------------

data Time = Time { getHours :: Hours
                 , getMinutes :: Minutes
                 , getSeconds :: Seconds
                 } deriving (Show, Read, Eq, Ord)

-- ---------------------------------------------------------------------------

data Year = Year { getYearNumber :: Int
                 , getEra :: Era
                 } deriving (Show, Read, Eq)

year :: Int -> Era -> Year
year x era
  | x < 0 = error (show x ++ " is not a valid year.")
  | otherwise = Year x era

instance Ord Year where
    (Year _ BC) `compare` (Year _ AD) = LT
    (Year _ AD) `compare` (Year _ BC) = GT
    (Year x BC) `compare` (Year y BC) = y `compare` x
    (Year x AD) `compare` (Year y AD) = x `compare` y 

data Era = BC | AD deriving (Show, Read, Eq, Ord, Bounded)

addYears :: Year -> Year -> Year
addYears (Year x eraX) (Year y eraY) = year (abs z) eraZ
    where z = yearValue x eraX + yearValue y eraY
          eraZ = if z >= 0 then AD else BC
          yearValue n era = case era of
                              BC -> (-n)
                              AD -> n

-- ---------------------------------------------------------------------------

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
           deriving (Show, Read, Eq, Ord, Enum, Bounded)

addMonths :: Month -> Month -> Month
addMonths = addEnums

-- ---------------------------------------------------------------------------

newtype Day = Day Int deriving (Show, Read, Eq, Ord)

instance Bounded Day where
    minBound = Day 0
    maxBound = Day 31

day :: Int -> Day
day x
  | d < minBound || d > maxBound = error (show x ++ " is not a valid day.")
  | otherwise = d
  where d = Day x

addDays :: Day -> Day -> Day
addDays (Day x) (Day y) = day $ (x + y) `mod` 31

-- ---------------------------------------------------------------------------

data DayOfWeek = Monday
               | Tuesday
               | Wednesday
               | Thursday
               | Friday
               | Saturday
               | Sunday
               deriving (Show, Read, Eq, Ord, Enum, Bounded)

addDaysOfWeek :: DayOfWeek -> DayOfWeek -> DayOfWeek
addDaysOfWeek = addEnums

-- ---------------------------------------------------------------------------

newtype Hours = Hours Int deriving (Show, Read, Eq, Ord)

instance Bounded Hours where
    minBound = Hours 0
    maxBound = Hours 23

hours :: Int -> Hours
hours x
  | h < minBound || h > maxBound = error (show x ++ " is not a valid hour.")
  | otherwise = h
  where h = Hours x

-- ---------------------------------------------------------------------------

newtype Minutes = Minutes Int deriving (Show, Read, Eq, Ord)

instance Bounded Minutes where
    minBound = Minutes 0
    maxBound = Minutes 59

minutes :: Int -> Minutes
minutes x
  | m < minBound || m > maxBound = error (show x ++ " is not a valid minute.")
  | otherwise = m
  where m = Minutes x

-- ---------------------------------------------------------------------------

newtype Seconds = Seconds Int deriving (Show, Read, Eq, Ord)

instance Bounded Seconds where
    minBound = Seconds 0
    maxBound = Seconds 59

seconds :: Int -> Seconds
seconds x
  | s < minBound || s > maxBound = error (show x ++ " is not a valid second.")
  | otherwise = s
  where s = Seconds x

-- ---------------------------------------------------------------------------

-- Add two enumerable values based on their "indicies" in an enumeration from
-- the minimum and maximum values of their type.
addEnums :: (Enum a, Bounded a, Eq a) => a -> a -> a
addEnums x y = succ $ range !! (i `mod` 12)
    where (Just i) = (+) <$> (x `elemIndex` range) <*> (y `elemIndex` range)
          range = [minBound .. maxBound]
