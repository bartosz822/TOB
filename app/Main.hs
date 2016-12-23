module Main where

import Lib

--Just for testing
import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf
import System.Time

server = "irc.freenode.org"
port   = 6667
chan   = "#BRbotTesting"
nick   = "BRBOT"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable config.
type Net = ReaderT Config IO
data Config = Config { socket :: Handle, starttime :: ClockTime }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = runReaderT run st

-- Connect to the server and return the initial bot state
connect :: IO Config
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber port)
    hSetBuffering h NoBuffering
    return (Config h t)
  where
    notify a = bracket_
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :tutorial bot")
    write "JOIN" chan
    asks socket >>= listen

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` liftIO (hGetLine h)
    liftIO (putStrLn s)
    if ping s then pong s else eval (clean s)
  where
    forever a = a >> forever a
    clean     = drop 1 . dropWhile (/= ':') . drop 1
    ping x    = "PING :" `isInfixOf` x
    pong x    = write  "PONG"  (':' : dropWhile( (/=) ':') x)

-- Parsing commands and running them
eval :: String -> Net ()
eval     "!quit"               = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
eval "!uptime" = uptime >>= privmsg
eval     _                     = return () -- ignore everything else

uptime :: Net String
uptime = do
    now  <- liftIO getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

--
-- Pretty print the date in '1d 9h 9m 17s' format
--
pretty :: TimeDiff -> String
pretty td =
  unwords $ map (uncurry (++) . first show) $
  if null diffs then [(0,"s")] else diffs
  where merge (tot,acc) (sec,typ) = let (sec',tot') = divMod tot sec
                                    in (tot',(sec',typ):acc)
        metrics = [(86400,"d"),(3600,"h"),(60,"m"),(1,"s")]
        diffs = filter ((/= 0) . fst) $ reverse $ snd $
                foldl' merge (tdSec td,[]) metrics


-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t

