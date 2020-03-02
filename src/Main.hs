{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import TrapGen.Types

import Data.Text (Text, pack) --, unpack, intercalate)
--import qualified Data.Text as T

--import System.Random (RandomGen, getStdGen)


main :: IO ()
main = pure "123" >>= putStrLn 



tx :: Show a => a -> Text
tx = pack . show

{-

renderCoord :: Int -> Point Int -> Text
renderCoord i pt = let i' = tx i 
                       (Point x' y') = tx <$> pt
                   in T.concat ["x", i', " = ", x', " y", i', " = ", y']


rotate :: Double -> Point Int -> Point Int
rotate a (Point x y) = let sina = sin a
                           cosa = cos a
                           x' = fromIntegral x
                           y' = fromIntegral y
                           xi = round $ x' * cosa - y' * sina
                           yi = round $ x' * sina + y' * cosa
                       in Point xi yi


offset :: Point Int -> Point Int -> Point Int
offset (Point x1 y1) (Point x2 y2) = Point (x1 + x2) (y1 + y2)


isoTilt :: Double -> Point Int -> Point Int
isoTilt t (Point x y) = Point x $ round $ t * (fromIntegral y)


kickItem :: Int -> [a] -> [a]
kickItem _ [] = []
kickItem 0 (_ : ls) = ls
kickItem i ls = let (xs, ys) = splitAt i ls in
                case ys of (_ : zs) -> xs ++ zs
                           [] -> xs


pickItems :: RandomGen g => Int -> [a] -> State g [a]
pickItems n tps = let ltps = length tps in
                  if (n >= ltps) then return tps
                  else if (n <= 0) then return []
                  else do idx <- mapM (\i -> genNumber $ Range 0 (i - 1)) [ltps, (ltps - 1) .. n + 1]
                          return $ foldl' (\xs d -> kickItem d xs) tps idx


pickItems' :: RandomGen g => Maybe Number -> [a] -> State g [a]
pickItems' Nothing tps = return tps
pickItems' (Just n') tps = do n <- genNumber n'
                              pickItems n tps

-}

