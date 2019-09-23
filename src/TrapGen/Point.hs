module TrapGen.Point where


import Data.Aeson ( withText
                  , FromJSON
                  , parseJSON
                  )

import Data.Text (unpack)

-- TODO: switch to attoparsec later

import Text.ParserCombinators.ReadPrec
import Text.Read


data Point a = Point a a deriving Eq


instance Show a => Show (Point a) where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"


instance Functor Point where
    fmap f (Point x y) = Point (f x) (f y)


instance Read a => Read (Point a) where
   readPrec = do x <- readPrec
                 c <- get
                 if (c /= ',') then fail "Failed to parse a pair: expected `,`"
                               else Point x <$> readPrec


instance Read a => FromJSON (Point a) where
    parseJSON = withText "Point" $ \t -> let s = unpack t in
                                         case readEither s of Right c -> return c
                                                              Left e -> fail $ "Coord (" ++ s ++ ") couldn't be parsed: " ++ e


