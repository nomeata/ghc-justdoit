module GHC.JustDoIt.Solver ( solve ) where

import Type
import CoreSyn

import GHC.LJT

import Data.Maybe

-- | Central place to plug additional solvers in. For now, we just do "GHC.LJT"
solve :: Type -> Maybe CoreExpr
solve = listToMaybe . ljt
