{-# LANGUAGE OverloadedStrings #-}

module TrapGen.Parser where


--import Data.Text (Text, unpack)
import System.IO (FilePath)


import TrapGen.Number
import TrapGen.Geometry
import TrapGen.AST


readAST :: FilePath -> IO IEML
readAST _ = return []



