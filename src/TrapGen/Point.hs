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


instance Applicative Point where
    pure x = Point x x
    (Point f g) <*> (Point x y) = Point (f x) (g y)


instance Foldable Point where
    foldMap fm (Point x y) = fm x <> fm y
    foldr f acc (Point x y) = f x $ f y acc


instance Traversable Point where
    traverse f (Point x y) = Point <$> f x <*> f y
    sequenceA (Point ax ay) = Point <$> ax <*> ay


instance Read a => Read (Point a) where
   readPrec = do x <- readPrec
                 c <- get
                 if (c /= ',') then fail "Failed to parse a pair: expected `,`"
                               else Point x <$> readPrec


instance Read a => FromJSON (Point a) where
    parseJSON = withText "Point" $ \t -> let s = unpack t in
                                         case readEither s of Right c -> return c
                                                              Left e -> fail $ "Coord (" ++ s ++ ") couldn't be parsed: " ++ e


