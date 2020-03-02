{-# LANGUAGE OverloadedStrings #-}

module TrapGen.AST where


import Data.Text (Text, unpack)


--import TrapGen.Number
--import TrapGen.Geometry


newtype ResourceName = ResourceName Text
                       deriving Eq


instance Show ResourceName where
    show (ResourceName t) = unpack t


data ValueExpr a = ValueScalar a
                 | ValueList [a]


data ValueBind = ValueBind Text ValueExpr





type IEML = [TopLevelExpr]


data TopLevelExpr = ValueBind
                  | EditArea ResourceName [AreaAction]
                  deriving Show


data AreaAction = AreaTrapAction [TrapAction]
                  deriving Show


data TrapAction = AddTrap
                | DeleteAllTraps
                deriving Show



