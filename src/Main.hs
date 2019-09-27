{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import TrapGen.Point
import TrapGen.Number
import TrapGen.Geometry
import TrapGen.InputDefinitions


import Control.Monad.Trans.State.Strict

import Data.Foldable (find, foldl')
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, intercalate)
import qualified Data.Text as T


import System.Environment (getArgs)
import System.IO (writeFile)
import System.Random (RandomGen, getStdGen)


main :: IO ()
--main = readParameters "./data.json" >>= putStrLn . show
--mai1 :: IO ()
main = do args <- getArgs
          case args of [a1, a2] -> writeTph a1 a2
                       _ -> error "Usage: trapgen <input path> <output path>"

  where writeTph :: FilePath -> FilePath -> IO ()
        writeTph fin fout = do ep <- readParameters fin
                               case ep of Left e -> error $ "JSON parse error: " ++ show e
                                          Right p -> do g <- getStdGen
                                                        let tph = evalState (genTph p) g 
                                                        writeFile fout (unpack tph)



genTph :: forall g . RandomGen g => Parameters -> State g Text
genTph (Parameters efs ars) = do txt <- mconcat <$> mapM genArea ars
                                 return $ "// Autogenerated by trapgen\n\n" <> txt

  where nest1 = "\n    "
        nest2 = "\n            "

       
        genArea :: Area -> State g Text
        genArea (Area (TextId a_id) clr grps) = do txt <- mconcat <$> mapM genGroup grps
                                                   return $ T.concat ["COPY_EXISTING ~", a_id, ".ARE~ ~override/", a_id, ".ARE~\n\n"
                                                                     , if clr then "LPF delete_normal_traps END\n\n" else mempty
                                                                     , txt
                                                                     ]

        genGroup :: TrapGroup -> State g Text
        genGroup (TrapGroup (TextId gid) tps pick) = do picked <- pickTraps
                                                        txt <- mconcat <$> mapM genTrap picked
                                                        return $ "// GROUP " <> gid <> ":\n\n" <> txt

          where genTrap :: Trap -> State g Text
                genTrap t = do verts <- genVerts (trap_geometry t)
                               script <- genScript (trap_effect t)
                               detect <- genNumber' (trap_detect t)
                               disarm <- genNumber' (trap_disarm t)
                               let (TextId trapid) = trap_id t
                               let prefix = T.take 31 $ T.concat ["trapgen_", gid, "_", trapid]
                               return $ T.concat [ "LPF add_are_trap"
                                                 , nest1, "INT_VAR trap_detect = ", detect, " trap_disarm = ", disarm
                                                 , nest2, verts
                                                 , nest1, "STR_VAR trap_script = ~", script, "~ trap_name = ~", prefix, "~\n"
                                                 , "END\n\n"
                                                 ]
                pickTraps :: State g [Trap]
                pickTraps = 
                  case pick of Nothing -> return tps
                               Just n' -> do n <- genNumber n'
                                             let ltps = length tps
                                             if (n >= ltps) then return tps
                                             else if (n <= 0) then return []
                                             else do idx <- mapM (\i -> genNumber $ Range 0 (i - 1)) [ltps, (ltps - 1) .. n + 1]
                                                     return $ foldl' (\xs d -> kickItem d xs) tps idx


        genVerts :: TrapGeometry -> State g Text

        genVerts (GeometryPoints pts) = intercalate nest2 <$> mapM (uncurry genCoord) (zip [0..] pts)
        genVerts (GeometryRect (Rectangle c w h a t)) = 
          do c' <- genPoint c
             (w1, w2) <- (`divMod` 2) <$> genNumber w
             (h1, h2) <- (`divMod` 2) <$> genNumber h
             a' <- (* pi) . (/ 180) . fromIntegral  <$> genNumber a
             t' :: Double <- (/ 10.0) . fromIntegral <$> genNumber t
             let left    = 0 - w1
                 top     = 0 - h1
                 right   = 0 + w1 + w2
                 bottom  = 0 + h1 + h2

                 move = offset c' . isoTilt t' . rotate a'

                 pts = move <$> [ Point left top
                                , Point right top
                                , Point right bottom
                                , Point left bottom
                                ]

             return $ intercalate nest2 $ uncurry renderCoord <$> zip [0..] pts


        genScript :: TrapEffect -> State g Text
        genScript (EffectFixed s) = return s
        genScript (EffectRandom f_id t_id) = 
            case find ((f_id==) . flavor_id) efs of 
                 Nothing -> error $ "Flavor '" ++ show f_id ++ "'not found"
                 Just f -> case find ((t_id==) . tier_id) (flavor_tiers f) of
                                Nothing -> error $ "Tier '" ++ show t_id ++ "' not found in flavor '" ++ show f_id ++ "'"
                                Just t -> let ss = tier_scripts t in
                                          do i <- genNumber $ Range 0 (length ss - 1)
                                             return $ ss !! i


        genPoint :: Point Number -> State g (Point Int)
        genPoint = traverse genNumber


        genNumber' :: Number -> State g Text
        genNumber' = fmap tx . genNumber


        genCoord :: Int -> Coord -> State g Text
        genCoord i pt = renderCoord i <$> genPoint pt




tx :: Show a => a -> Text
tx = pack . show


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



