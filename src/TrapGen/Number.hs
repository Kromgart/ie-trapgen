module TrapGen.Number where

import Data.Aeson ( withText
                  , FromJSON
                  , parseJSON
                  )

import Data.Text (unpack)

-- TODO: switch Read to megaparsec later

import Text.ParserCombinators.ReadPrec
import Text.Read


data Number = Strict Int
            | Range Int Int
            | Delta Int Int
            deriving (Show, Eq)


getRange :: Number -> (Int, Int)
getRange (Strict x) = (x, x)
getRange (Range m n) = (m, n)
getRange (Delta x dx) = (x - dx, x + dx)


solveNumber :: Number -> Int
solveNumber (Strict x) = x
solveNumber (Range m n) = error "Not implemented"
solveNumber (Delta x dx) = error "Not implemented"



readStrict :: ReadPrec Number
readStrict = Strict <$> readPrec


readRange :: ReadPrec Number
readRange = do m <- readPrec
               c <- get
               if c == '-' then Range m <$> readPrec
                           else fail $ "Failed to parse a Range Number: unexpected symbol " ++ show c


readDelta :: ReadPrec Number
readDelta = do x <- readPrec
               c <- get
               if c == '~' then Delta x <$> readPrec
                           else fail $ "Failed to parse a Delta Number: unexpected symbol " ++ show c


instance Read Number where
   readPrec = readRange <++ readDelta <++ readStrict


instance FromJSON Number where
    parseJSON = withText "Number" $ \t -> case (readEither $ unpack t) of Right n -> pure n
                                                                          Left e -> fail e
 

