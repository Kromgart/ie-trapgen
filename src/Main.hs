{-# LANGUAGE OverloadedStrings #-}

module Main where

import TrapGen.Point
import TrapGen.Number
import TrapGen.InputDefinitions
import Data.List ()
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack, intercalate)

import System.Environment (getArgs)
import System.IO (writeFile)


main :: IO ()
--main = readFile "./data.json" >>= putStrLn
--main2 :: IO ()
main = do args <- getArgs
          case args of [a1, a2] -> fdo a1 a2
                       _ -> error "Usage: trapgen <input path> <output path>"

  where fdo fin fout = do ep <- readParameters fin
                          case ep of Left e -> putStrLn $ "JSON parse error: " ++ show e
                                     Right p -> let tph = genTph p in
                                                writeFile fout (unpack tph)



genTph :: Parameters -> Text
genTph (Parameters ars) = mconcat $ processArea <$> ars
  where
    processArea :: Area -> Text
    processArea (Area aid grps) = "COPY_EXISTING ~" <> aid <> ".ARE~ ~override/" <> aid <> ".ARE~\n\n" <> (mconcat $ processGroup <$> grps)
      where
        processGroup :: TrapGroup -> Text
        processGroup (TrapGroup gid trps) = mconcat $ processTrap <$> trps
          where 
            processTrap :: Trap -> Text
            processTrap (Trap { trap_id = tid, trap_detect = tdet, trap_disarm = tdis, trap_effect = teff, trap_geometry = tgeo }) =
                "LPF add_are_trap\n" <> 
                "    INT_VAR " <> verts <> "\n" <>
                "    STR_VAR trap_script = ~" <> script <> "~\n" <>
                "END\n\n"
              where verts = case tgeo of GeometryPoints pts -> intercalate "\n            " $ renderCoord <$> zip [0..] pts

                    script = case teff of EffectFixed s -> s

                    tx = pack . show
                
                    renderCoord :: (Int, Coord) -> Text
                    renderCoord (i, (Point x y)) = "x"<> i' <> " = " <> x' <> " y" <> i' <> " = " <> y'
                      where i' = tx i
                            x' = tx $ solveNumber x
                            y' = tx $ solveNumber y

