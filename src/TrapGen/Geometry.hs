{-# LANGUAGE OverloadedStrings #-}

module TrapGen.Geometry where


import TrapGen.Number
import TrapGen.Point


type Coord = Point Number


data Rectangle = Rectangle { center :: Coord
                           , width  :: Number
                           , height :: Number
                           , angle  :: Number
                           , tilt   :: Number
                           }
                           deriving Show


data TrapGeometry = GeometryPoints [Coord]
                  | GeometryRect Rectangle
                  deriving Show




