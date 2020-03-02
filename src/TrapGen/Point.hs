module TrapGen.Point where


import Data.Text (unpack)

-- TODO: switch to megaparsec later

import Text.ParserCombinators.ReadPrec
import Text.Read


data Point a = Point a a deriving Eq


instance Show a => Show (Point a) where
    show (Point x y) = "(" ++ show x ++ "," ++ show y ++ ")"


instance Semigroup a => Semigroup (Point a) where
    (Point x1 y1) <> (Point x2 y2) = Point (x1 <> x2) (y1 <> y2)


instance Monoid a => Monoid (Point a) where
    mempty = Point mempty mempty


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



