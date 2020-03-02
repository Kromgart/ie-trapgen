{-# LANGUAGE TemplateHaskell #-}

module TrapGen.Types.TH where


import Language.Haskell.TH

import TrapGen.Types 
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty)
import Control.Monad ((<=<))


data Enum a => RndOrd a = Exact a
                        | Range a a
                        | Delta a Int
                        deriving (Show)


data Foldable a => RndSomeOf a b = All (a b)
                                 | SomeOf (a b) (RndOrd Int)
                                 deriving (Show)


data RndOneOf a = Single a
                | OneOf (NonEmpty a)
                deriving (Show)





mkRndName :: Name -> Name
mkRndName n = mkName $ nameBase n ++ "Rnd"


isEnum :: Type -> Q Bool
isEnum t = not . null <$> reifyInstances ''Enum [t]


isFoldable :: Type -> Q Bool
isFoldable t = not . null <$> reifyInstances ''Foldable [t]


mkRndTypes :: [Name] -> Q [Dec]
mkRndTypes tadd = traverse mkRndType tadd
  where
    mkRndType :: Name -> Q Dec
    mkRndType n = do inf <- reify n
                     let drv = [ DerivClause Nothing [ ConT ''Show ] ]
                     case inf of (TyConI dc) -> case dc of (DataD cx n' tvb mkn cs' _) -> do cs <- traverse mkCon cs'
                                                                                             return $ DataD cx (mkRndName n') tvb mkn cs drv
                                                           (NewtypeD cx n' tvb mkn cs' _) -> do cs <- mkCon cs'
                                                                                                return $ NewtypeD cx (mkRndName n') tvb mkn cs drv

                                                           _ -> fail $ "Unsupported type declaration " ++ show dc
                                 tt -> fail $ "mkTypeRnd: unsupported type " ++ (show tt)


    mkCon :: Con -> Q Con
    mkCon (NormalC n bt) = NormalC (mkRndName n) <$> traverse (\(tb, t) -> (,) tb <$> mkRndSubtype t) bt
    mkCon (RecC n bt) = RecC (mkRndName n) <$> traverse (\(tn, tb, t) -> (,,) tn tb <$> mkRndSubtype t) bt
    mkCon _ = fail $ "mkCon: unsupported constructor type"


    quantifySubtype :: Type -> Q Type
    quantifySubtype t = do b <- isFoldable t
                           if b then [t| RndSomeOf $(pure t) |]
                                else [t| RndOneOf  $(pure t) |]

    mkRndSubtype :: Type -> Q Type
    mkRndSubtype ListT = [t| RndSomeOf [] |]
    mkRndSubtype t@(ConT n) | elem n tadd = return $ ConT $ mkRndName n
                            | otherwise = do b <- isEnum t
                                             if b then [t| RndOrd $(pure t) |]
                                                  else return t

    mkRndSubtype (AppT t1 t2) = AppT <$> mkRndSubtype t1 <*> mkRndSubtype t2
    mkRndSubtype st = fail $ "mkRndSubtype: unsupported type of subtype " ++ show st


