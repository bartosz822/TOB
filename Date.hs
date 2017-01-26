import Data.Time.Clock
import Data.Time.Calendar
import Data.Time
import Data.Time.Clock.POSIX

 -- | retruns current date (year, month, day)
date :: IO Day
date = getCurrentTime >>= return . utctDay

checkHowLong :: String -> IO (Maybe Integer)
checkHowLong y = do
	day <- date
	return $  do 
		newDate <- parseDate y
		return $ diffDays (chooseDate newDate) day


parseDate :: String -> Maybe Importantdates
parseDate "wielkanoc" = Just Easter
parseDate "ferie zimowe" = Just Winterhollidays
parseDate "ferie letnie" = Just Summerholidays
parseDate "sesja zimowa" = Just Startwintersession
parseDate "sesja letnia" = Just Startsummersession
parseDate _ = Nothing

-- enum type 
data Importantdates = Easter |
					 Startwintersession |
					 Startsummersession | 
					 Summerholidays |
 					 Winterhollidays

-- | pisze
-- asdasd
-- adsda
chooseDate :: Importantdates -> Day
chooseDate Easter = fromGregorian 2017 04 16
chooseDate Startwintersession = fromGregorian 2017 01 28
chooseDate Winterhollidays = fromGregorian 2017 02 20
chooseDate Summerholidays =  fromGregorian 2017 07 01
chooseDate Startsummersession = fromGregorian 2017 06 17


--formatTime :: FormatTime t => TimeLocale -> String -> t -> String
time = getZonedTime

selectData n x y z = return (addDays n (fromGregorian x y z))

