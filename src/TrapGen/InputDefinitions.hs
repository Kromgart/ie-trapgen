{-# LANGUAGE OverloadedStrings #-}

module TrapGen.InputDefinitions where

import Data.Aeson ( withObject
                  , withText
                  , FromJSON
                  , parseJSON
                  , (.:)
                  , (.:?)
                  , eitherDecode
                  )


import qualified Data.ByteString.Lazy as BS
import Data.Text (Text, unpack)
import System.IO (FilePath)


import TrapGen.Number
import TrapGen.Point


type Coord = Point Number


newtype TextId a = TextId Text
                   deriving Eq

instance Show (TextId a) where
    show (TextId t) = unpack t

instance FromJSON (TextId a) where
    parseJSON = withText "TextId" $ return . TextId
                   


data TrapEffect = EffectFixed Text
                | EffectRandom (TextId EffectFlavor) (TextId EffectTier)
                deriving Show


instance FromJSON TrapEffect where
    parseJSON = withObject "TrapEffect" $ \v -> do t <- v .: "type"
                                                   case t of "fixed" -> EffectFixed <$> v .: "script"
                                                             "random" -> EffectRandom <$> v .: "flavor_id"
                                                                                      <*> v .: "tier_id"

                                                             u -> fail $ "Unknown TrapEffect type " ++ u



data TrapGeometry = GeometryPoints [Coord]
                    deriving Show


instance FromJSON TrapGeometry where
    parseJSON = withObject "TrapGeometry" $ \v -> do t <- v .: "type"
                                                     case t of "points" -> GeometryPoints <$> v .: "points"
                                                               u -> fail $ "Unknown TrapGeometry type " ++ u

data Trap = Trap { trap_id :: TextId Trap
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



data TrapGroup = TrapGroup { group_id :: TextId TrapGroup
                           , group_traps :: [Trap]
                           , group_pick :: Maybe Number
                           }
                           deriving Show


instance FromJSON TrapGroup where
    parseJSON = withObject "TrapGroup" $ \v -> TrapGroup <$> v .: "id"
                                                         <*> v .: "traps"
                                                         <*> v .:? "pick"


data Area = Area { area_id :: TextId Area
                 , area_groups :: [TrapGroup]
                 }
                 deriving Show


instance FromJSON Area where
    parseJSON = withObject "Area" $ \v -> Area <$> v .: "id"
                                               <*> v .: "groups"


data EffectTier = EffectTier { tier_id :: TextId EffectTier
                             , tier_scripts :: [Text]
                             }
                             deriving Show


instance FromJSON EffectTier where
    parseJSON = withObject "Tier" $ \v -> EffectTier <$> v .: "id"
                                                     <*> v .: "scripts"


data EffectFlavor = EffectFlavor { flavor_id :: TextId EffectFlavor
                                 , flavor_tiers :: [EffectTier]
                                 }
                                 deriving Show


instance FromJSON EffectFlavor where
    parseJSON = withObject "Flavor" $ \v -> EffectFlavor <$> v .: "flavor"
                                                         <*> v .: "tiers"



data Parameters = Parameters [EffectFlavor] [Area]
                  deriving Show


instance FromJSON Parameters where
    parseJSON = withObject "Parameters" $ \v -> Parameters <$> v .: "random_effects"
                                                           <*> v .: "areas"


readParameters :: FilePath -> IO (Either String Parameters)
readParameters p = eitherDecode <$> BS.readFile p


