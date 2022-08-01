module GHC.JustDoIt.Solver ( solve ) where

import Data.Maybe
import GHC.Plugins

import GHC.LJT


-- | Central place to plug additional solvers in. For now, we just do "GHC.LJT"
solve :: Type -> Maybe CoreExpr
solve = listToMaybe . ljt
