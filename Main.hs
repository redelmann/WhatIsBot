module Main(main) where

import Network
import Network.IRC hiding (nick, user)
import System.IO

import Control.Monad (forever, replicateM_, mapM_, when)
import Control.Monad.Reader hiding (reader)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan

import Data.Char (isSpace)
import Data.List (isPrefixOf, intercalate, break, splitAt)
import Data.Monoid (mempty)
import Data.Maybe (mapMaybe)
import qualified Data.Map as M

import Options.Applicative hiding (reader)

import Classifier

-- | User speciable options.
data Options = Options 
    { server   :: String
    , port     :: Integer
    , chan     :: String
    , user     :: String
    , nick     :: String }

-- | Parser for the options.
optionsParser :: Parser Options
optionsParser = Options <$> strOption (short 's' <> long "server" <> metavar "SERVER")
                        <*> option (short 'p' <> long "port" <> metavar "PORT")
                        <*> strOption (short 'c' <> long "channel" <> metavar "CHAN")
                        <*> strOption (short 'u' <> long "user" <> metavar "USER")
                        <*> strOption (short 'n' <> long "nickname" <> metavar "NICK")

-- | Informations needed by the Bot.
data Infos = Infos 
    { inChan  :: Chan Message
    , outChan :: Chan Message
    , models  :: M.Map String Model
    , options :: Options }

-- | The Bot type, a wrapper around IO with access to the informations.
type Bot a = ReaderT Infos IO a

-- | Runs an IRC bot to classify starcraft II player names.
main :: IO ()
main = do
    i <- execParser opts >>= setup
    runReaderT run i
  where
    opts = (info (optionsParser <**> helper) mempty)

-- | Reader process. Reads from the handle and write corresponding messages in a `Chan`.
reader :: Handle -> Chan Message -> IO ()
reader handle outC = do
    ls <- lines <$> hGetContents handle
    mapM_ (probe "< " encode >=> writeChan outC) $ mapMaybe decode ls

-- | Writer process. Reads messages from a `Chan` and writes them to the handle.
writer :: Handle -> Chan Message -> IO ()
writer handle inC = do
    msgs <- getChanContents inC
    mapM_ ((probe "> " id >=> hPutStrLn handle) . encode) msgs

-- | Print out some informations, returns its initial value untouched.
probe :: MonadIO m => String -> (a -> String) -> a -> m a
probe s f m = do
    liftIO $ putStrLn $ s ++ f m
    return m

-- | Setup the Bot. Builds the models, connect to the server and launches the reader and writer processes. 
setup :: Options -> IO Infos
setup os = do
    -- Building models.
    ms <- learn $ M.fromList 
        [ ("a Pokemon", "resources/pokemons1st.txt")
        , ("a pharmaceutical product", "resources/pharma.txt")
        , ("an ancient historian", "resources/historians.txt") ]

    -- Connecting to the specified IRC server.
    h <- connectTo (server os) (PortNumber (fromIntegral $ port os))

    -- Don't want to map /r/n to /n on input due to IRC parser.
    -- But output should map /n to /r/n.
    hSetNewlineMode h $ NewlineMode LF CRLF 
    
    -- No buffering.
    hSetBuffering h NoBuffering

    inC <- newChan
    outC <- newChan
    forkIO $ reader h inC
    forkIO $ writer h outC
    return $ Infos inC outC ms os

-- | Main body of the bot. Does initial communication with the server, 
--   connects to the specified channel and then answer to requests.
run :: Bot ()
run = do
    -- Getting the stream of incomming messages.
    inC <- inChan <$> ask
    (initMsgs, msgs) <- break isEndInit <$> liftIO (getChanContents inC) 
    -- Setting up nick and user.
    setNick
    setUser
    -- Process messages of the initialisation.
    mapM_ processMessage initMsgs
    -- Connect to the specified channel.
    setChan
    -- | Greet everyone!
    printGreetings
    -- Process all messages.
    mapM_ processMessage msgs
  where
    -- We declare the end of the initialization when we receive a MODE message.
    isEndInit :: Message -> Bool
    isEndInit m = case m of
        Message _ "MODE" _ -> True
        _                  -> False

-- | Sends out the nickname to the server.
setNick :: Bot ()
setNick = do
    n <- nick . options <$> ask
    write $ Message Nothing "NICK" [n]

-- | Sends out the username to the server.
setUser :: Bot ()
setUser = do
    n <- user . options <$> ask
    write $ Message Nothing "USER" [n, "*", "*", "What Is Bot"]

-- | Connects to the specified channel.
setChan :: Bot ()
setChan = do
    c <- chan . options <$> ask
    write $ Message Nothing "JOIN" [c]

-- | Prints greetings for everyone to see.
printGreetings :: Bot ()
printGreetings = do
    c <- chan . options <$> ask
    send c $ "Hello everyone! Whisper me a starcraft user name, and I'll let you know what it refers to!"

-- | Writes a message to the IRC server.
write :: Message -> Bot ()
write msg = do
    ch <- outChan <$> ask
    liftIO $ writeChan ch msg

-- | Sends a message to some user or channel.
send :: String -> String -> Bot ()
send to msg = do
    u <- user . options <$> ask
    n <- nick . options <$> ask
    write $ Message (Just $ NickName n (Just u) Nothing) "PRIVMSG" [to, msg]

-- | Ping back the server, with specified parameters.
pong :: [String] -> Bot ()
pong ps = write $ Message Nothing "PONG" ps

-- | Removes spaces from the begining and the end of the string.
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

-- | Process a message.
--   If the message is a ping, a pong is replied.
--   If the message is a private message to the bot, 
--   then a reply is sent, with the classification of the provided message.
--   Otherwise, the message is ignored.
processMessage :: Message -> Bot ()
processMessage m = case m of
    Message _ "PING" ps   -> pong ps  -- We must answer to pings.
    Message (Just (NickName from _ _)) "PRIVMSG" [to, txt] -> do
        n <- nick . options <$> ask
        when (to == n) $ do
            -- We received a private request.
            ms <- models <$> ask
            send from $ classify (trim txt) ms
    _ -> return ()
