-- Code formatted using stylish Haskell
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- = Weather module used for getting current weather information for Krakow

module Weather
  (
    getWeatherKrk,
    jsonURL,
    getJSON,
    getTemp,
    getPressure,
    getHumidity,
    Weather
  ) where



import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           GHC.Generics
import           Network.HTTP.Conduit (simpleHttp)

-- | OpenWeatherMap API URL for Krakow
jsonURL :: String
jsonURL = "http://api.openweathermap.org/data/2.5/weather?q=Krakow,uk&appid=bf5a5bd346890c81c4bea71ed5bc62b5&units=metric"

-- | Downloads json from jsonURL
getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL


-- | Data type containing information about current weather
data Weather = Weather{
    temp     :: Double,
    pressure :: Double,
    humidity :: Double
} deriving (Show)


-- | In order to use decode function Weather needs to be a member of FromJSON typeclass.
instance FromJSON Weather where
    -- | parseJSON extracts current weather information from a JSON Bytestring and packs it inside Weather datatype
    parseJSON (Object v) =
        Weather <$>  (inside >>= (.: "temp"))
            <*>  (inside >>= (.: "pressure"))
            <*>  (inside >>= (.: "humidity"))
            where inside = v .: "main"
    parseJSON _ = mzero




-- | Returns string containing information about current weather in Krakow. Weather is downloaded from openWeathermap API.
-- Because it's downloaded from the internet it's wrapped in IO.
getWeatherKrk :: IO String
getWeatherKrk = let w = decode <$> getJSON :: IO (Maybe Weather) in do
    tempW <- getTemp <$> w
    pressureW <- getPressure <$> w
    humidityW <- getHumidity <$> w
    return $ "Pogoda w Krakowie: Temperatura : " ++ tempW ++ " Cisnienie: " ++ pressureW ++ " Wilgotnosc: " ++ humidityW

-- | Extracts temperature information from result of decoding JSON
getTemp :: Maybe Weather -> String
getTemp (Just x) = (show . temp $ x) ++ " stopni C"
getTemp Nothing  = "Nie udało się odczytać"

-- | Extracts pressure information from result of decoding JSON
getPressure :: Maybe Weather -> String
getPressure (Just x) = (show . pressure $ x) ++ "hPa"
getPressure Nothing  = "Nie udało się odczytać"

-- | Extracts humidity information from result of decoding JSON
getHumidity :: Maybe Weather -> String
getHumidity (Just x) = (show . humidity $ x) ++ "%"
getHumidity Nothing  = "Nie udało się odczytac"
