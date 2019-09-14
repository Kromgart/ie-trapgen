module TrapGen.Number where

import Data.Aeson ( withText
                  , FromJSON
                  , parseJSON
                  )

import Data.Text (unpack)
import Control.Monad.Trans.State.Strict (State, state)
import System.Random (RandomGen, randomR)

-- TODO: switch Read to attoparsec later
import Text.ParserCombinators.ReadPrec (ReadPrec, (<++))
import Text.Read (readPrec, readEither, get)


data Number = Strict Int
            | Range Int Int
            | Delta Int Int
            deriving (Show, Eq)


genNumber :: RandomGen g => Number -> State g Int
genNumber (Strict x)   = return x
genNumber (Range m n)  = state $ randomR (m, n) 
genNumber (Delta x dx) = state $ randomR (x - dx, x + dx)


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
 

