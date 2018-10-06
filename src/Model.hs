module Model where

data MovingState =
    MovingForward
  | MovingBackward
  | MovingLeft
  | MovingRight
  | NotMoving

data Movement =
    ForwardMovement Int
  | BackwardMovement Int
  | TurnMovement Int

newtype PosX = PosX Double deriving (Eq, Show)
newtype PosY = PosY Double deriving (Eq, Show)

data Position = Position PosX PosY deriving (Eq, Show)

data RoverState = RoverState
  { _movingState :: MovingState
  , _movements   :: [Movement]
  , _position    :: Position
  }
