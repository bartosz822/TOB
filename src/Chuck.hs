{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- = module used to receive a random Chack Norris quote
module Chuck
  (
   getQuote,
   jsonURL,
   getJSON,
   getValue,
   Quote
  )where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import           Data.Version
import           GHC.Generics
import           Network.HTTP.Conduit (simpleHttp)

-- | Chuck Norris quotes url
jsonURL :: String
jsonURL = "https://api.chucknorris.io/jokes/random"

-- | Downloads json from jsonURL
getJSON :: IO B.ByteString
getJSON = simpleHttp jsonURL

-- | Data type containing a quote
data Quote = Quote {
    value :: String
    } deriving(Show)


-- | In order to use decode function getQuote needs to be a member of FromJSON typeclass.
instance FromJSON Quote where
    parseJSON (Object v) =
        Quote <$>  v .: "value"
    parseJSON _ = mzero


-- | Returns string
-- Because it's downloaded from the internet it's wrapped in IO.
getQuote :: IO String
getQuote = let w = decode <$> getJSON :: IO (Maybe Quote) in
    getValue <$> w


-- | Extracts quote from result of decoding JSON
getValue :: Maybe Quote -> String
getValue (Just x) = show . value $ x
getValue Nothing  =  "Nie udało się odczytać"


