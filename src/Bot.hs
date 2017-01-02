module Bot
(
    Net,
    Config,
    socket,
    handle,
    connect,
    run,
 ) where

import           Control.Arrow
import           Control.Exception
import           Control.Monad.Reader
import           Data.List
import           Network
import           System.Exit
import           System.IO
import           System.Time

server = "irc.freenode.org"
port   = 6667
chan   = "#BRbotTesting"
nick   = "TOBbot"
password = "tob12345"

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable config.
type Net = ReaderT Config IO
data Config = Config { socket :: Handle, starttime :: ClockTime }

-- Connect to the server and return the initial bot state
connect :: IO Config
connect = notify $ do
    t <- getClockTime
    h <- connectTo server (PortNumber port)
    hSetBuffering h NoBuffering
    hSetBuffering stdout LineBuffering
    return (Config h t)
  where
    notify  = bracket_
        (putStrLn $ "Connecting to" ++ server ++ " ... " )
        (putStrLn "done.")


-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands

run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :tutorial bot")
    write "JOIN" chan
    write "/msg NickServ identify" password
--     liftIO $ hFlush stdout
    asks socket >>= listen

-- Process each line from the server
listen :: Handle -> Net ()
listen h = forever $ do
    s <-  liftIO (hGetLine h)
    liftIO (putStrLn s)
--     liftIO $ hFlush stdout
    if ping s then pong s else eval (clean s)
    where
        clean     = drop 1 . dropWhile (/= ':') . drop 1
        ping x    = "PING" `isPrefixOf` x
        pong x    = write  "PONG"  (drop 4 x)


-- Parsing commands and running them
eval :: String -> Net ()
eval x           | "!id " `isPrefixOf` x = privmsg (drop 4 x)
                 | "!uptime" `isPrefixOf` x   = uptime >>= privmsg
                 | "!quit" `isPrefixOf` x = write "QUIT" ":Exiting" >> liftIO exitSuccess
eval     _       = return () -- ignore everything else



uptime :: Net String
uptime = do
    now  <- liftIO getClockTime
    zero <- asks starttime
    return . pretty $ diffClockTimes now zero

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)


-- Send a message out to the server we're currently connected to
write :: String -> String -> Net ()
write command text = do
    h <- asks socket
    liftIO $ hPutStr h $ command ++ " " ++ text ++ "\r\n"
    liftIO $ putStrLn  $ command ++ " " ++ text


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
