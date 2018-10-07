module Lib( run) where

import           Control.Concurrent       (Chan, forkIO)
import           Control.Concurrent.Async (async, race, waitAnyCancel)
import qualified Control.Concurrent.Chan  as Chan
import           Control.Exception        (finally)
import           Control.Monad            (forever)
import           Control.Monad.State
import           Controller
import           Events
import           Gui                      (DataPoint (..), GuiChannel,
                                           createGui)
import           Network                  (PortID (..), connectTo,
                                           withSocketsDo)
import           System.IO

run :: IO ()
run = withSocketsDo $ do
    handle <- connectTo "127.0.0.1" (PortNumber 5000)
    guiMsgChannel <- Chan.newChan
    talk guiMsgChannel handle `finally` hClose handle

talk :: GuiChannel -> Handle -> IO ()
talk guiMsgChannel handle = do
    hSetNewlineMode handle universalNewlineMode
    hSetBuffering handle LineBuffering
    fromServerAsync <- async fromServer
    toServerAsync <- async toServer
    guiAsync <- async $ Gui.createGui guiMsgChannel
    _ <- waitAnyCancel [fromServerAsync, toServerAsync, guiAsync]
    return ()
    where
        fromServer = evalStateT (fromServerLoop guiMsgChannel handle) (RoverState False)
        toServer = do
            line <- getLine
            case line of
    -- server accepts /quit as disconnect command so better send it to the server
                ":q" -> do hPutStrLn handle "/quit"; return ()
                _    ->  do hPutStrLn handle line; toServer

fromServerLoop :: GuiChannel -> Handle -> StateT RoverState IO ()
fromServerLoop guiMsgChannel handle = forever $ do
    line <- lift $ hGetLine handle
    case stringToEvent line of
        Right event -> do
            action <- onEvent event
            state <- get
            lift (eventToUI guiMsgChannel event)
            return ()
        Left e      -> do
            lift (print line)
            lift (print e)

eventToUI :: GuiChannel -> Event -> IO ()
eventToUI guiMsgChannel (AxlEvent t (X value) _ _) = Chan.writeChan guiMsgChannel (DataPoint t value)
eventToUI _ _ = return ()
