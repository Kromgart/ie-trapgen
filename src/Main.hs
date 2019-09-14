{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import TrapGen.Point
import TrapGen.Number
import TrapGen.InputDefinitions

import Control.Monad.Trans.State.Strict
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, intercalate)

import System.Environment (getArgs)
import System.IO (writeFile)
import System.Random (RandomGen, getStdGen)


main :: IO ()
--main = readFile "./data.json" >>= putStrLn
--main2 :: IO ()
main = do args <- getArgs
          case args of [a1, a2] -> writeTph a1 a2
                       _ -> error "Usage: trapgen <input path> <output path>"

  where writeTph :: FilePath -> FilePath -> IO ()
        writeTph fin fout = do ep <- readParameters fin
                               case ep of Left e -> putStrLn $ "JSON parse error: " ++ show e
                                          Right p -> do g <- getStdGen
                                                        let tph = evalState (genTph p) g 
                                                        writeFile fout (unpack tph)



genTph :: forall g . RandomGen g => Parameters -> State g Text
genTph (Parameters ars) = do txt <- mconcat <$> mapM genArea ars
                             return $ "// Autogenerated by trapgen\n\n" <> txt

  where nest1 = "\n    "
        nest2 = "\n            "
       
        genArea :: Area -> State g Text
        genArea (Area a_id grps) = do txt <- mconcat <$> mapM genGroup grps
                                      return $ "COPY_EXISTING ~" <> a_id <> ".ARE~ ~override/" <> a_id <> ".ARE~\n\n" <> txt


        genGroup :: TrapGroup -> State g Text
        genGroup (TrapGroup gid trps) = do txt <- mconcat <$> mapM genTrap trps
                                           return $ "// GROUP " <> gid <> ":\n\n" <> txt


        genTrap :: Trap -> State g Text
        genTrap t = do verts <- genVerts (trap_geometry t)
                       script <- genScript (trap_effect t)
                       detect <- genNum (trap_detect t)
                       disarm <- genNum (trap_disarm t)
                       return $  "LPF add_are_trap" <> 
                                 nest1 <> "INT_VAR trap_detect = " <> detect <> " trap_disarm = " <> disarm <> 
                                 nest2 <> verts <>
                                 nest1 <> "STR_VAR trap_script = ~" <> script <> "~\n" <>
                                 "END\n\n"

        genVerts :: TrapGeometry -> State g Text
        genVerts (GeometryPoints pts) = intercalate nest2 <$> mapM genCoord (zip [0..] pts)

        genCoord :: (Int, Coord) -> State g Text
        genCoord (i, (Point x y)) = do x' <- genNum x 
                                       y' <- genNum y
                                       let i' = tx i
                                       return $ "x" <> i' <> " = " <> x' <> " y" <> i' <> " = " <> y'

        genScript :: TrapEffect -> State g Text
        genScript (EffectFixed s) = return s

        tx = pack . show

        genNum :: Number -> State g Text
        genNum = fmap tx . genNumber



