{-# LANGUAGE OverloadedStrings #-}

module TrapGen.InputDefinitions where

import Data.Aeson ( withObject
                  , withText
                  , FromJSON
                  , parseJSON
                  , (.:)
                  , (.:?)
                  , (.!=)
                  , eitherDecode
                  )


import qualified Data.ByteString.Lazy as BS
import Data.Text (Text, unpack)
import System.IO (FilePath)


import TrapGen.Number
import TrapGen.Geometry



newtype TextId a = TextId Text
                   deriving Eq

instance Show (TextId a) where
    show (TextId t) = unpack t

instance FromJSON (TextId a) where
    parseJSON = withText "TextId" $ return . TextId
                   


data TrapEffect = EffectFixed Text
                | EffectRandom (TextId EffectGroup)
                deriving Show


instance FromJSON TrapEffect where
    parseJSON = withObject "TrapEffect" $ \v -> do t <- v .: "type"
                                                   case t of "fixed" -> EffectFixed <$> v .: "script"
                                                             "random" -> EffectRandom <$> v .: "group_id"

                                                             u -> fail $ "Unknown TrapEffect type '" ++ u ++ "'"



data Trap = Trap { trap_id       :: TextId Trap
                 , trap_detect   :: Number
                 , trap_disarm   :: Number
                 , trap_effect   :: TrapEffect
                 , trap_geometry :: TrapGeometry
                 }
                 deriving Show


instance FromJSON Trap where
    parseJSON = withObject "Trap" $ \v -> Trap <$> v .: "id" 
                                               <*> v .: "detect"
                                               <*> v .: "disarm"
                                               <*> v .: "effect"
                                               <*> v .: "geometry"



data TrapGroup = TrapGroup { group_id    :: TextId TrapGroup
                           , group_traps :: [Trap]
                           , group_pick  :: Maybe Number
                           }
                           deriving Show


instance FromJSON TrapGroup where
    parseJSON = withObject "TrapGroup" $ \v -> TrapGroup <$> v .: "id"
                                                         <*> v .: "traps"
                                                         <*> v .:? "pick"


data Area = Area { area_id     :: TextId Area
                 , area_clear  :: Bool
                 , area_pick   :: Maybe Number
                 , area_groups :: [TrapGroup]
                 }
                 deriving Show


instance FromJSON Area where
    parseJSON = withObject "Area" $ \v -> Area <$> v .: "id"
                                               <*> v .:? "clear" .!= False
                                               <*> v .:? "pick"
                                               <*> v .: "groups"


data EffectGroup = EffectGroup { effectgroup_id      :: TextId EffectGroup
                               , effectgroup_scripts :: [Text]
                               }
                               deriving Show


instance FromJSON EffectGroup where
    parseJSON = withObject "EffectGroup" $ \v -> EffectGroup <$> v .: "id"
                                                             <*> v .: "scripts"




data Parameters = Parameters [EffectGroup] [Area]
                  deriving Show


instance FromJSON Parameters where
    parseJSON = withObject "Parameters" $ \v -> Parameters <$> v .: "random_effects"
                                                           <*> v .: "areas"


readParameters :: FilePath -> IO (Either String Parameters)
readParameters p = eitherDecode <$> BS.readFile p


