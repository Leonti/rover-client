module Controller where

import           Control.Monad.State
import           Events

data RoverState = RoverState
  { buttonPressed :: Bool
  }

type Counter = Integer

data Action = None | SomeAction String deriving Show

onEvent :: Monad m => Event -> StateT RoverState m Action
onEvent event = do
    oldState <- get
    put (RoverState True)
    return $ SomeAction "send a message"
