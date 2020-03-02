{-# LANGUAGE TemplateHaskell #-}

module TrapGen.Types.TH where


import Language.Haskell.TH

import TrapGen.Types 
--import Data.Text (Text)


mkRndName :: Name -> Name
mkRndName n = mkName $ nameBase n ++ "Rnd"



mkRndTypes :: [Name] -> Q [Dec]
mkRndTypes = traverse mkRndType
  where
    mkRndType :: Name -> Q Dec
    mkRndType n = do inf <- reify n
                     case inf of (TyConI dc) -> case dc of (DataD cx n' tvb mkn cs' drv) -> do cs <- traverse mkCon cs'
                                                                                               return $ DataD cx (mkRndName n') tvb mkn cs drv
                                                           (NewtypeD cx n' tvb mkn cs' drv) -> do cs <- mkCon cs'
                                                                                                  return $ NewtypeD cx (mkRndName n') tvb mkn cs drv

                                                           _ -> fail $ "Unsupported type declaration " ++ show dc
                                 tt -> fail $ "mkTypeRnd: unsupported type " ++ (show tt)


    mkCon :: Con -> Q Con
    mkCon (NormalC n bt) = NormalC (mkRndName n) <$> traverse (traverse mkRndSubtype ) bt
    mkCon (RecC n bt) = RecC (mkRndName n) <$> traverse (\(tn,tb,t) -> (,,) tn tb <$> mkRndSubtype t) bt
    mkCon _ = fail $ "mkCon: unsupported constructor type"


    mkRndSubtype :: Type -> Q Type
    mkRndSubtype ListT = return ListT
    mkRndSubtype (ConT n) = case (show n) of "GHC.Types.Int" -> [t| ROrd Int |]
                                             "Data.Text.Internal.Text" -> return $ ConT n
                                             _ -> return $ ConT $ mkRndName n

    mkRndSubtype (AppT t1 t2) = AppT <$> mkRndSubtype t1 <*> mkRndSubtype t2
    mkRndSubtype st = fail $ "mkRndSubtype: unsupported type of subtype " ++ show st


