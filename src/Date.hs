
-- |
-- = module used to reveive number of days between today and a specyfic date
module Date(
        date,
        checkHowLong,
        parseDate,
        chooseDate,
        getDifference
        )where

import           Data.Time
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX

 -- | retruns current date (year, month, day)
date :: IO Day
date =  utctDay <$> getCurrentTime

-- | compute how many days is between two dates
checkHowLong :: String -> IO (Maybe Integer)
checkHowLong y = do
        day <- date
        return $  do
                newDate <- parseDate y
                return $ diffDays (chooseDate newDate) day

getDifference :: String -> IO String
getDifference x = checkHowLong x >>= \y -> case y of
    Nothing ->  return "Nie mamy takiej daty sprobuj: wielkanoc, ferie_zimowe, ferie_letnie, sesja_zimowa, sesja_letnia"
    (Just a) -> return $ "Do " ++ x ++ " pozostalo : " ++ show a ++ " dni"


-- | parse input
parseDate :: String -> Maybe Importantdates
parseDate "wielkanoc"    = Just Easter
parseDate "ferie_zimowe" = Just Winterhollidays
parseDate "ferie_letnie" = Just Summerholidays
parseDate "sesja_zimowa" = Just Startwintersession
parseDate "sesja_letnia" = Just Startsummersession
parseDate _              = Nothing

-- enum type
data Importantdates = Easter |
                      Startwintersession |
                      Startsummersession |
                      Summerholidays |
                      Winterhollidays

-- | list of possible dates
chooseDate :: Importantdates -> Day
chooseDate Easter             = fromGregorian 2017 04 16
chooseDate Startwintersession = fromGregorian 2017 01 28
chooseDate Winterhollidays    = fromGregorian 2017 02 20
chooseDate Summerholidays     =  fromGregorian 2017 07 01
chooseDate Startsummersession = fromGregorian 2017 06 17



