module Gui(createGui, GuiChannel, DataPoint(..)) where
import           Control.Monad

import           Control.Concurrent
import qualified Control.Concurrent.Chan     as Chan

import           Control.Monad.State
import           Events                      (Timestamp)
import qualified Graphics.UI.Threepenny      as UI
import           Graphics.UI.Threepenny.Core hiding (get)

type GuiChannel = Chan DataPoint

data DataPoint = DataPoint Timestamp Double deriving Show

newtype GuiState = GuiState
  { dataPoints :: [DataPoint] }

newtype X = X Integer deriving Show
newtype Y = Y Integer deriving Show
data Coordinate = Coordinate X Y deriving Show

toCoordinates :: [DataPoint] -> [Coordinate]
toCoordinates dps = coordinates
    where
      (coordinates, _) = foldr dpToCoordinate ([], Nothing) dps

canvasHeight = 1000

dpToCoordinate :: DataPoint -> ([Coordinate], Maybe Timestamp) -> ([Coordinate], Maybe Timestamp)
dpToCoordinate (DataPoint ts v) (coords, Just ts') = (Coordinate (X (max 0 (ts - ts'))) (Y $ abs $ scaleValue canvasHeight 0.3 v) : coords, Just ts')
dpToCoordinate (DataPoint ts v) (coords, Nothing) = (Coordinate (X 0) (Y $ scaleValue 200 0.5 v) : coords, Just ts)

createGui :: Chan DataPoint -> IO ()
createGui msgs = startGUI defaultConfig $ setup msgs

scaleValue :: Int -> Double -> Double -> Integer
scaleValue scaleMax maxUnscaled value = round $ (value / maxUnscaled) * fromIntegral scaleMax

drawGraph :: [Coordinate] -> UI.Canvas -> UI ()
drawGraph coords canvas = do
  canvas # UI.beginPath
--  canvas # UI.moveTo (0, 0)
  canvas # set' UI.strokeStyle "green"
  forM_ coords (\(Coordinate (X x) (Y y)) -> canvas # UI.lineTo (fromIntegral x - 60, fromIntegral (fromIntegral canvasHeight - y)))
  canvas # UI.stroke


drawState :: GuiState -> UI.Canvas -> UI ()
drawState st canvas = do
  canvas # UI.clearCanvas
  drawGraph (toCoordinates $ take 35 $ dataPoints st) canvas
  flushCallBuffer -- make sure that JavaScript functions are executed

processMessage :: Window -> UI.Canvas -> DataPoint -> StateT GuiState IO ()
processMessage w canvas msg = do
  _ <- modify (\s -> GuiState { dataPoints = msg : dataPoints s })
  state <- get
--  lift $ print $ show (take 20 $ dataPoints state)
  lift $ runUI w (drawState state canvas)


receiveMessages :: Window -> GuiChannel -> UI.Canvas -> IO ()
receiveMessages w msgs canvas = do
    _ <- putStrLn "receiving messages"
    messages <- Chan.getChanContents msgs
    evalStateT (forM_ messages (processMessage w canvas)) GuiState { dataPoints = [] }

setup :: GuiChannel -> Window -> UI ()
setup globalMsgs window = do
    msgs <- liftIO $ Chan.dupChan globalMsgs
    return window # set title "Rover - debug"

    canvas <- UI.canvas
        # set UI.height canvasHeight
        # set UI.width  2000
        # set style [("border", "solid black 1px"), ("background", "#eee")]

    messageReceiver <- liftIO $ forkIO $ receiveMessages window msgs canvas
    on UI.disconnect window $ const $ liftIO $ killThread messageReceiver

    getBody window #+
        [ column [element canvas]
        ]

    return ()
