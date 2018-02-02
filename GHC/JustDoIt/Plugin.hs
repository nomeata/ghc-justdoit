{-# LANGUAGE CPP, TupleSections #-}
module GHC.JustDoIt.Plugin ( plugin )
where

-- external
import Data.Maybe
import Control.Monad

-- GHC API
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Plugins    (Plugin (..), defaultPlugin)
import TcEvidence
import TcPluginM
import TcRnTypes
import Class
import CoreUtils
import MkCore
import TyCon
import Type
import CoreSyn

import GHC.JustDoIt.Solver

plugin :: Plugin
plugin = defaultPlugin { tcPlugin = const (Just jdiPlugin) }

jdiPlugin :: TcPlugin
jdiPlugin =
  TcPlugin { tcPluginInit  = lookupJDITyCon
           , tcPluginSolve = solveJDI
           , tcPluginStop  = const (return ())
           }

lookupJDITyCon :: TcPluginM Class
lookupJDITyCon = do
    Found _ md   <- findImportedModule jdiModule Nothing
    jdiTcNm <- lookupOrig md (mkTcOcc "JustDoIt")
    tcLookupClass jdiTcNm
  where
    jdiModule  = mkModuleName "GHC.JustDoIt"

wrap :: Class -> CoreExpr -> EvTerm
wrap cls = EvExpr . appDc
  where
    tyCon = classTyCon cls
    dc = tyConSingleDataCon tyCon
    appDc x = mkCoreConApps dc [Type (exprType x), x]

findClassConstraint :: Class -> Ct -> Maybe (Ct, Type)
findClassConstraint cls ct = do
    (cls', [t]) <- getClassPredTys_maybe (ctPred ct)
    guard (cls' == cls)
    return (ct, t)

solveJDI :: Class -- ^ JDI's TyCon
         -> [Ct]  -- ^ [G]iven constraints
         -> [Ct]  -- ^ [D]erived constraints
         -> [Ct]  -- ^ [W]anted constraints
         -> TcPluginM TcPluginResult
solveJDI jdiCls _ _ wanteds =
    return $! case result of
        Left x       -> TcPluginContradiction [x]
        Right solved -> TcPluginOk solved []
  where
    our_wanteds = mapMaybe (findClassConstraint jdiCls) wanteds
    result = partitionMaybe (fmap (wrap jdiCls) . solve) our_wanteds

partitionMaybe :: (b -> Maybe c) -> [(a,b)] -> Either a [(c,a)]
partitionMaybe _ [] = Right []
partitionMaybe f ((k,v):xs) = case f v of
    Nothing -> Left k
    Just y  -> ((y,k):) <$> partitionMaybe f xs
