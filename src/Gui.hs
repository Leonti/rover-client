module Gui(createGui) where
import           Control.Monad

import           Control.Concurrent
import qualified Control.Concurrent.Chan     as Chan

import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core


createGui :: Chan String -> IO ()
createGui msgs = startGUI defaultConfig $ setup msgs

canvasSize = 600

receiveMessages :: Window -> Chan String -> UI.Canvas -> IO ()
receiveMessages w msgs canvas = do
    messages <- Chan.getChanContents msgs
    forM_ messages $ \msg -> runUI w $ do
          canvas # UI.clearCanvas
          canvas # UI.fillText msg (140,60)
          flushCallBuffer -- make sure that JavaScript functions are executed

setup :: Chan String -> Window -> UI ()
setup globalMsgs window = do
    msgs <- liftIO $ Chan.dupChan globalMsgs
    return window # set title "Rover - debug"

    canvas <- UI.canvas
        # set UI.height canvasSize
        # set UI.width  canvasSize
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    messageReceiver <- liftIO $ forkIO $ receiveMessages window msgs canvas
    on UI.disconnect window $ const $ liftIO $ killThread messageReceiver

    getBody window #+
        [ column [element canvas]
        ]

    return ()
