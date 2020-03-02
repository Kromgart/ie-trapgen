{-# LANGUAGE TemplateHaskell #-}

module TrapGen.Types.Randomized where


import TrapGen.Types 
import TrapGen.Types.TH

--import Language.Haskell.TH
--import Data.Text (Text)


$(mkRndTypes [ ''Point
             , ''Rect
             , ''Geometry
             , ''Script
             , ''Trap
             ])

