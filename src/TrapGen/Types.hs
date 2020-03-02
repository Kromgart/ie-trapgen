{-# LANGUAGE OverloadedStrings #-}

module TrapGen.Types where


--import TrapGen.Number
--import TrapGen.Point

import Data.Text (Text)

--type Coord = Point Number


data Enum a => ROrd a = Exact a
                      | Range a a
                      | Delta a Int
                      deriving (Show)


data RQuant a = One a
              | All [a]
              | SomeOf [a] Int
              deriving (Show)


data Point = Point { x :: Int
                   , y :: Int
                   }
                   deriving (Show)


data Rect = Rect { center :: Point
                 , width  :: Int
                 , height :: Int
                 , angle  :: Int
                 , scaleY :: Int
                 }
                 deriving (Show)


data Geometry = Points [Point]
              | Rectangle Rect
              deriving (Show)


newtype Script = Script { unScript :: Text }
                 deriving (Show)



data Trap = Trap { detect :: Int
                 , disarm :: Int
                 , effect :: Script
                 , geometry :: Geometry
                 }
                 deriving (Show)


newtype TrapGroup = TrapGroup [Trap]






