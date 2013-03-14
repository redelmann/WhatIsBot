{-# LANGUAGE DoAndIfThenElse #-}

module Main(main) where

import Control.Monad (forever, replicateM_)
import Control.Monad.Reader
import qualified Data.Map as M
import Network
import System.IO
import Text.Printf
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Monoid (mempty)
import Options.Applicative

import Classifier

data Options = Options 
    { server   :: String
    , port     :: Integer
    , chan     :: String
    , user     :: String
    , nick     :: String }

optionsParser :: Parser Options
optionsParser = Options <$> strOption (short 's' <> long "server" <> metavar "SERVER")
                        <*> option (short 'p' <> long "port" <> metavar "PORT")
                        <*> strOption (short 'c' <> long "channel" <> metavar "CHAN")
                        <*> strOption (short 'u' <> long "user" <> metavar "USER")
                        <*> strOption (short 'n' <> long "nickname" <> metavar "NICK")

data Infos = Infos 
    { handle  :: Handle
    , models  :: M.Map String Model
    , options :: Options }

type Bot = ReaderT Infos IO

main :: IO ()
main = do
    i <- execParser opts >>= getInfos
    runReaderT run i
  where
    opts = (info (optionsParser <**> helper) mempty)


greetings :: Bot ()
greetings = privmsg ("Hello everyone! Is this weird name a Pokemon, " ++
                     "a pharmaceutical product or an ancient historian? " ++
                     "Type \"!whatis? name\" to get a definitive answer!")

getInfos :: Options -> IO Infos
getInfos os = do
    ms <- learn $ M.fromList 
        [ ("a Pokemon", "resources/pokemons1st.txt")
        , ("a pharmaceutical product", "resources/pharma.txt")
        , ("an ancient historian", "resources/historians.txt") ]
    h <- connectTo (server os) (PortNumber (fromIntegral $ port os))
    hSetBuffering h NoBuffering
    return $ Infos h ms os

run :: Bot ()
run = do
    os <- options <$> ask
    write "NICK" $ nick os
    write "USER" (user os ++ " * * :What is this?")
    waitOK
    write "JOIN" $ chan os
    greetings
    listen

waitOK :: Bot ()
waitOK = do
    h <- handle <$> ask
    t <- liftIO $ hGetLine h
    let s = init t
    liftIO $ putStrLn s
    if ping s then do
        pong s
        waitOK 
    else if notice s then
        waitOK
    else
        return ()
  where 
    notice x = "NOTICE" `isPrefixOf` x
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

write :: String -> String -> Bot ()
write s t = do
    h <- handle <$> ask
    liftIO $ do
        hPrintf h "%s %s\r\n" s t
        printf    "> %s %s\n" s t

privmsg :: String -> Bot ()
privmsg s = do
    os <- options <$> ask
    write "PRIVMSG" (chan os ++ " :" ++ s)
 
listen :: Bot ()
listen = do
    h <- handle <$> ask
    forever $ do
        t <- liftIO $ hGetLine h
        let s = init t
        liftIO $ putStrLn s
        if ping s then pong s else eval (clean s)
  where 
    clean     = drop 1 . dropWhile (/= ':') . drop 1
 
    ping x    = "PING :" `isPrefixOf` x
    pong x    = write "PONG" (':' : drop 6 x)

eval :: String -> Bot ()
eval s | "!whatis? " `isPrefixOf` s = do
    let w = trim $ drop 9 s
    m <- models <$> ask
    privmsg $ classify w m
eval _ = return ()

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
