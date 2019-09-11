{-# LANGUAGE OverloadedStrings #-}

module TrapGen.InputDefinitions where

import Data.Aeson ( withObject
                  , withText
                  , withArray
                  , FromJSON
                  , parseJSON
                  , Value
                  , (.:)
                  , eitherDecode
                  )

import Data.Aeson.Types ( Parser )                  

import qualified Data.ByteString.Lazy as BS
import Data.Text (Text, unpack)
import Data.Vector (toList)
import System.IO (FilePath)
import Text.Read (readEither)


import TrapGen.Number
import TrapGen.Point


type Coord = Point Number


data TrapEffect = EffectFixed Text
                  deriving Show


instance FromJSON TrapEffect where
    parseJSON = withObject "TrapEffect" $ \v -> do t <- v .: "type"
                                                   case t of "fixed" -> EffectFixed <$> v .: "script"
                                                             u -> fail $ "Unknown TrapEffect type " ++ u



data TrapGeometry = GeometryPoints [Coord]
                    deriving Show


instance FromJSON TrapGeometry where
    parseJSON = withObject "TrapGeometry" $ \v -> do t <- v .: "type"
                                                     case t of "points" -> GeometryPoints <$> v .: "points"
                                                               u -> fail $ "Unknown TrapGeometry type " ++ u

data Trap = Trap { trap_id ::Text
                 , trap_detect :: Number
                 , trap_disarm :: Number
                 , trap_effect :: TrapEffect
                 , trap_geometry :: TrapGeometry
                 }
                 deriving Show


instance FromJSON Trap where
    parseJSON = withObject "Trap" $ \v -> Trap <$> v .: "id" 
                                               <*> v .: "detect"
                                               <*> v .: "disarm"
                                               <*> v .: "effect"
                                               <*> v .: "geometry"



data TrapGroup = TrapGroup { group_id :: Text
                           , group_traps :: [Trap]
                           }
                           deriving Show


instance FromJSON TrapGroup where
    parseJSON = withObject "TrapGroup" $ \v -> TrapGroup <$> v .: "id"
                                                         <*> v .: "traps"


data Area = Area { area_id :: Text
                 , area_groups :: [TrapGroup]
                 }
                 deriving Show


instance FromJSON Area where
    parseJSON = withObject "Area" $ \v -> Area <$> v .: "id"
                                               <*> v .: "groups"


data Parameters = Parameters [Area]
                  deriving Show


instance FromJSON Parameters where
    parseJSON = withObject "Parameters" $ \v -> Parameters <$> v .: "areas"


readParameters :: FilePath -> IO (Either String Parameters)
readParameters p = eitherDecode <$> BS.readFile p


