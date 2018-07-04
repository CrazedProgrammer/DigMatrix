{-# LANGUAGE DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}

module Blocks
    ( Block
    , Scan
    , addBlocks
    , getBlocks
    , saveBlocks
    , scanBlocks
    ) where

import GHC.Generics (Generic)
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Control.Monad.IO.Class
import Linear.V3
import Linear.Metric (distance)
import Data.String
import Data.Maybe

data Block = Block
  { blockId     :: String
  , blockDamage :: Int
  , blockPosition :: V3 Int
  } deriving (Eq, Generic, Show)

data Scan = Scan
  { scanId :: String
  , scanDamage :: Int
  , scanOrigin :: V3 Int
  , scanRange :: Float
  } deriving (Eq, Generic, Show)

instance ToJSON Block where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Block

instance ToJSON Scan where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Scan

instance (Num n, ToJSON n) => ToJSON (V3 n) where
  toJSON (V3 x y z) = object [ "x" .= x, "y" .= y, "z" .= z ]

instance (FromJSON n, Num n) => FromJSON (V3 n) where
  parseJSON = withObject "V3" $ \v -> V3 <$> v .: "x" <*> v .: "y" <*> v .: "z"

dataFile :: String
dataFile = "data.json"

addBlocks :: [Block] -> [Block] -> [Block]
addBlocks xs ys =
  (filter (\(Block _ _ position) -> not (elem position newPositions)) xs) ++ ys
    where
      newPositions = map (\(Block _ _ position) -> position) ys

getBlocks :: MonadIO m => m [Block]
getBlocks = do
  fileContent <- liftIO (B.readFile dataFile)
  pure (fromMaybe [] (decode fileContent :: Maybe [Block]))

saveBlocks :: MonadIO m => [Block] -> m ()
saveBlocks blocks = do
  liftIO (B.writeFile dataFile (encode blocks))
  liftIO (putStrLn "Saving blocks:")
  liftIO (B.putStrLn (encode blocks))
  pure ()


scanBlockMatch :: Scan -> Block -> Bool
scanBlockMatch (Scan scanId scanDamage scanOrigin scanRange) (Block blockId blockDamage blockPosition) =
  scanId == blockId
  && scanDamage == blockDamage
  && inRange blockPosition scanOrigin scanRange
    where
      inRange position origin range = distance (fromIntegral <$> position) (fromIntegral <$> origin) <= range

scanBlocks :: [Scan] -> [Block] -> [Block]
scanBlocks scans = filter (\block -> any (flip scanBlockMatch block) scans)

