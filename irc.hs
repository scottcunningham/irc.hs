import Network
import System.IO
import Text.Printf
import Data.List
import System.Exit
import Control.Arrow
import Control.Exception
import Prelude hiding (catch)
import Control.Monad.Reader

server = "irc.freenode.org"
port = 6667
chan = "#botwars"
nick = "currybot"

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

main :: IO ()
main = bracket connect disconnect loop
    where
        disconnect = hClose . socket
        loop st = catch (runReaderT run st) (\(SomeException _) -> return ())

connect :: IO Bot
connect = notify $ do
        h <- connectTo server (PortNumber (fromIntegral port))
        hSetBuffering h NoBuffering
        return (Bot h)
    where
        notify a = bracket_
            (printf "Connecting to %s ..." server >> hFlush stdout)
            (putStrLn "done")
            a

run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick++" 0 * :scott bot")
    write "JOIN" chan
    asks socket >>= listen

write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    liftIO $ hPrintf h "%s %s\r\n" s t
    liftIO $ printf ">>> %s %s\r\n" s t

listen :: Handle -> Net ()
listen h = forever $ do
        s <- init `fmap` liftIO (hGetLine h)
        if ping s then pong s else eval (clean s)
        liftIO $ putStrLn s
    where
        forever a = do a; forever a
        clean = drop 1 . dropWhile (\x -> x /= ":") . drop 1 
        ping x 	  = "PING :" `isPrefixOf` x
        pong x    = write "PONG" (':' : drop 6 x)


eval :: String -> Net ()
eval "?quit"        = write "QUIT" ":Exiting" >> liftIO (exitWith ExitSuccess)
eval x | "?id" `isPrefixOf` x = privmsg (drop 4 x)
eval  _ = return ()

privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

