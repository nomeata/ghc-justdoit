-- | An implementation of LJT proof search directly on Core terms.
module GHC.LJT where

import FastString
import Unique
import Type
import Id
import CoreSyn
import Outputable
import TyCoRep
import TyCon
import DataCon
import MkCore
import CoreUtils
import TysWiredIn
import BasicTypes

import Data.List
import Data.Hashable
import Control.Monad
import Data.Bifunctor

ljt ::  Type -> [CoreExpr]
ljt t = [] ==> t


newVar :: Type -> Id
newVar ty = mkSysLocal (mkFastString "x") (mkBuiltinUnique i) ty
  where i = hash (showSDocUnsafe (ppr ty))


(==>) :: [Id] -> Type -> [CoreExpr]

-- Rule Axiom
-- (TODO: Why restrict to atoms?)
ante ==> goal
    | Just v <- find (\v -> idType v `eqType` goal) ante
    = pure $ Var v

-- Rule f⇒
-- (TODO: Why restrict to atoms?)
ante ==> goal
    | Just v <- find (\v -> isEmptyTy (idType v)) ante
    = pure $ mkWildCase (Var v) (idType v) goal []

-- Rule →⇒2
ante ==> goal
    | Just ((v,((tys, build, _destruct),_r)),ante') <- anyA (funLeft isProdType) ante
    = let vs = map newVar tys
          expr = mkLams vs (App (Var v) (build (map Var vs)))
          v' = newVar (exprType expr)
      in mkLetNonRec v' expr <$> (v' : ante') ==> goal

-- Rule →⇒3
ante ==> goal
    | Just ((v,((tys, injs, _destruct),_r)),ante') <- anyA (funLeft isSumType) ante
    = let es = [ lam ty (\vx -> App (Var v) (inj (Var vx))) | (ty,inj) <- zip tys injs ]
      in letsA es $ \vs -> (vs ++ ante') ==> goal

-- Rule ∧⇒
ante ==> goal
    | Just ((v,(tys, _build, destruct)),ante') <- anyA isProdType ante
    = let pats = map newVar tys
      in destruct (Var v) pats <$> (pats ++ ante') ==> goal

-- Rule ⇒∧
ante ==> goal
    | Just (tys, build, _destruct) <- isProdType goal
    = build <$> sequence [ante ==> ty | ty <- tys]

-- Rule ∨⇒
ante ==> goal
    | Just ((vAorB, (tys, _injs, destruct)),ante') <- anyA isSumType ante
    = let vs = map newVar tys in
      destruct (Var vAorB) vs <$> sequence [ (v:ante') ==> goal | v <- vs]

-- Rule ⇒→
ante ==> FunTy t1 t2
    = Lam v <$> (v : ante) ==> t2
  where
    v = newVar t1

-- Rule →⇒1
ante ==> goal
    | let isInAnte a = find (\v -> idType v `eqType` a) ante
    , Just ((vAB, (vA,_)), ante') <- anyA (funLeft isInAnte) ante
    = letA (App (Var vAB) (Var vA)) $ \vB -> (vB : ante') ==> goal

-- Rule ⇒∨
ante ==> goal
    | Just (tys, injs, _destruct) <- isSumType goal
    = msum [ inj <$> ante ==> ty | (ty,inj) <- zip tys injs ]

-- Rule →⇒4
ante ==> goal
    | Just ((vABC, ((a,b),_)), ante') <- anyA (funLeft (funLeft Just)) ante
    = do
        let eBC = lam b $ \vB -> App (Var vABC) (lam a $ \_ -> Var vB)
        eAB <- letA eBC           $ \vBC -> (vBC : ante') ==> FunTy a b
        letA (App (Var vABC) eAB) $ \vC  -> (vC : ante') ==> goal

-- Nothing found :-(
_ante ==> _goal
    = -- pprTrace "go" (vcat [ ppr (idType v) | v <- ante] $$ text "------" $$ ppr goal) $
      mzero

-- A typed lambda
lam :: Type -> (Id -> CoreExpr) -> CoreExpr
lam ty gen = Lam v $ gen v
  where v = newVar ty

lamA :: Applicative f => Type -> (Id -> f CoreExpr) -> f CoreExpr
lamA ty gen = Lam v <$> gen v
  where v = newVar ty

let_ :: CoreExpr -> (Id -> CoreExpr) -> CoreExpr
let_ e gen = mkLetNonRec v e $ gen v
  where v = newVar (exprType e)

letA :: Applicative f => CoreExpr -> (Id -> f CoreExpr) -> f CoreExpr
letA e gen = mkLetNonRec v e <$> gen v
  where v = newVar (exprType e)

letsA :: Applicative f => [CoreExpr] -> ([Id] -> f CoreExpr) -> f CoreExpr
letsA es gen = mkLets (zipWith NonRec vs es) <$> gen vs
  where vs = map (newVar . exprType) es

isProdType :: Type -> Maybe ([Type], [CoreExpr] -> CoreExpr, CoreExpr -> [Id] -> CoreExpr -> CoreExpr)
isProdType ty
    | Just (_, _, dc, repargs) <- splitDataProductType_maybe ty
    = Just ( repargs
           , \args -> mkConApp dc (map Type repargs ++ args)
           , \scrut pats rhs -> mkWildCase scrut ty (exprType rhs) [(DataAlt dc, pats, rhs)]
           )
isProdType _ = Nothing

-- Haskell sum constructors can have multiple parameters. For our purposes, if so, we wrap them in a product.
isSumType :: Type -> Maybe ([Type], [CoreExpr -> CoreExpr], CoreExpr -> [Id] -> [CoreExpr] -> CoreExpr)
isSumType ty
    | Just (tycon, ty_args) <- splitTyConApp_maybe ty
    , Just dcs <- isDataSumTyCon_maybe tycon
    = let tys = [ mkTupleTy Boxed (dataConInstArgTys dc ty_args) | dc <- dcs ]
          injs = [
            let vtys = dataConInstArgTys dc ty_args
                vs = map newVar vtys
            in \ e -> mkSmallTupleCase vs (mkConApp dc (map Type ty_args ++ map Var vs))
                        (mkWildValBinder (exprType e)) e
           | dc <- dcs]
          destruct = \e vs alts ->
            Case e (mkWildValBinder (exprType e)) (exprType (head alts)) 
            [ let pats = map newVar (dataConInstArgTys dc ty_args) in
              (DataAlt dc, pats, mkLetNonRec v (mkCoreVarTup pats) rhs)
            | (dc,v,rhs) <- zip3 dcs vs alts ]
      in Just (tys, injs, destruct)
isSumType _ = Nothing

funLeft :: (Type -> Maybe a) -> Type -> Maybe (a,Type)
funLeft p (FunTy t1 t2) = (\x -> (x,t2)) <$> p t1
funLeft _ _ = Nothing

anyA :: (Type -> Maybe a) -> [Id] -> Maybe ((Id, a), [Id])
anyA _ [] = Nothing
anyA p (v:vs) | Just x <- p (idType v) = Just ((v,x), vs)
              | otherwise              = second (v:) <$> anyA p vs
