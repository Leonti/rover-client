{-# LANGUAGE OverloadedStrings #-}

module Events where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString.Lazy.UTF8 (fromString)

data Side = LeftSide | RightSide deriving Show
type Timestamp = Integer
type EventType = String

newtype IrFrontLeft = IrFrontLeft Bool deriving Show
newtype IrFrontRight = IrFrontRight Bool deriving Show
newtype IrRightFront = IrRightFront Bool deriving Show
newtype IrRightRear = IrRightRear Bool deriving Show
newtype IrRearRight = IrRearRight Bool deriving Show
newtype IrRearLeft = IrRearLeft Bool deriving Show
newtype IrLeftRear = IrLeftRear Bool deriving Show
newtype IrLeftFront = IrLeftFront Bool deriving Show

newtype Voltage = Voltage Double deriving Show
newtype CurrentMa = CurrentMa Double deriving Show

newtype BatteryTemp = BatteryTemp Double deriving Show
newtype AmbientTemp = AmbientTemp Double deriving Show

newtype X = X Double deriving Show
newtype Y = Y Double deriving Show
newtype Z = Z Double deriving Show

data Event =
    EncoderEvent Timestamp Side
  | IrSensorEvent Timestamp IrFrontLeft IrFrontRight IrRightFront IrRightRear IrRearRight IrRearLeft IrLeftRear IrLeftFront
  | BatteryEvent Timestamp Voltage CurrentMa
  | TempEvent Timestamp BatteryTemp AmbientTemp
  | ButtonEvent Timestamp
  | CompassEvent Timestamp X Y Z
  | GyroEvent Timestamp X Y Z
  | AxlEvent Timestamp X Y Z
  deriving Show

instance FromJSON Event where
  parseJSON = withObject "event" $ \o -> do
      time <- o .: "time"
      eventType <- o .: "type" :: Parser EventType
      parseEvent time eventType o

parseEvent :: Timestamp -> EventType -> Object -> Parser Event
parseEvent time "ENCODER" o = do
  sideString <- o .: "value" :: Parser String
  side <- case sideString of
              "LEFT"  -> return LeftSide
              "RIGHT" -> return RightSide
              _       -> fail $ "Unknown side '" ++ sideString ++ "'"
  return $ EncoderEvent time side
parseEvent time "IR_SENSOR" o = IrSensorEvent time
  <$> (IrFrontLeft <$> (v >>= (.: "front") >>= (.: "left")))
  <*> (IrFrontRight <$> (v >>= (.: "front") >>= (.: "right")))
  <*> (IrRightFront <$> (v >>= (.: "right") >>= (.: "front")))
  <*> (IrRightRear <$> (v >>= (.: "right") >>= (.: "rear")))
  <*> (IrRearRight <$> (v >>= (.: "rear") >>= (.: "right")))
  <*> (IrRearLeft <$> (v >>= (.: "rear") >>= (.: "left")))
  <*> (IrLeftRear <$> (v >>= (.: "left") >>= (.: "rear")))
  <*> (IrLeftFront <$> (v >>= (.: "left") >>= (.: "front")))
  where
    v = o .: "value"
parseEvent time "BATTERY" o = BatteryEvent time
  <$> (Voltage <$> ((o .: "value") >>= (.: "voltage")))
  <*> (CurrentMa <$> ((o .: "value") >>= (.: "current_mA")))
parseEvent time "TEMP" o = TempEvent time
  <$> (BatteryTemp <$> ((o .: "value") >>= (.: "battery")))
  <*> (AmbientTemp <$> ((o .: "value") >>= (.: "ambient")))
parseEvent time "COMPASS" o = CompassEvent time
  <$> (X <$> ((o .: "value") >>= (.: "x")))
  <*> (Y <$> ((o .: "value") >>= (.: "y")))
  <*> (Z <$> ((o .: "value") >>= (.: "z")))
parseEvent time "GYRO" o = GyroEvent time
  <$> (X <$> ((o .: "value") >>= (.: "x")))
  <*> (Y <$> ((o .: "value") >>= (.: "y")))
  <*> (Z <$> ((o .: "value") >>= (.: "z")))
parseEvent time "AXL" o = AxlEvent time
  <$> (X <$> ((o .: "value") >>= (.: "x")))
  <*> (Y <$> ((o .: "value") >>= (.: "y")))
  <*> (Z <$> ((o .: "value") >>= (.: "z")))
parseEvent time "BUTTON" _ = return $ ButtonEvent time
parseEvent _ _ o = fail $ "Unknown event: " ++ show o

stringToEvent :: String -> Either String Event
stringToEvent text = eitherDecode (fromString text) :: Either String Event
