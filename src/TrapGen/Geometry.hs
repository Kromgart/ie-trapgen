{-# LANGUAGE OverloadedStrings #-}

module TrapGen.Geometry where


import Data.Aeson ( withObject
                  , FromJSON
                  , parseJSON
                  , (.:)
                  )


import TrapGen.Number
import TrapGen.Point


type Coord = Point Number


data Rectangle = Rectangle { center :: Coord
                           , width  :: Number
                           , height :: Number
                           , angle  :: Number
                           }
                           deriving Show


data TrapGeometry = GeometryPoints [Coord]
                  | GeometryRect Rectangle
                  deriving Show


instance FromJSON TrapGeometry where
    parseJSON = withObject "TrapGeometry" $ \v -> do t <- v .: "type"
                                                     case t of "points" -> GeometryPoints <$> v .: "points"
                                                               "rectangle" -> GeometryRect <$> (Rectangle <$> v .: "center"
                                                                                                          <*> v .: "width"
                                                                                                          <*> v .: "height"
                                                                                                          <*> v .: "angle"
                                                                                               )

                                                               u -> fail $ "Unknown TrapGeometry type '" ++ u ++ "'"




