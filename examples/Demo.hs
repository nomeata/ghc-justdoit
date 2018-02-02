{-# OPTIONS_GHC -fplugin=GHC.JustDoIt.Plugin #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, EmptyCase #-}
import GHC.JustDoIt
import Test.Inspection

import Prelude hiding (id, const, curry)

data Unit = Unit

id :: a -> a
id = justDoIt

const :: a -> b -> a
const = (…)

dup :: a -> (a,a)
dup = (…)

pair :: a -> b -> (a,b)
pair = (…)

tripl :: a -> b -> c -> (a,b,c)
tripl = (…)

proj :: (a,b,c,d) -> c
proj = (…)

curry :: ((a,b) -> c) -> a -> b -> c
curry = (…)

curry2 :: ((a,b) -> c) -> b -> a -> c
curry2 = (…)

contBind :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
contBind = (…)

unit :: Unit
unit = (…)

swapEither :: Either a b -> Either b a
swapEither = (…)

swapEitherCont :: (((Either a b) -> r) -> r) -> (((Either b a) -> r) -> r)
swapEitherCont = (…)

randomCrap :: (a -> b) -> (a,c,d) -> (d,b,b)
randomCrap = (…)

data Void

absurd :: Void -> a
absurd = (…)



id' x = x
const' x _= x
dup' x = (x,x)
pair' x y = (x,y)
tripl' x y z = (x,y,z)
proj' (_,_,c,_) = c
curry' f a b = f (a,b)
curry2' f a b = f (b,a)
unit' = Unit
contBind' :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
contBind' ca cb k = ca (\a -> cb a k)
swapEither' (Left a) = (Right a)
swapEither' (Right a) = (Left a)
swapEitherCont' :: (((Either a b) -> r) -> r) -> (((Either b a) -> r) -> r)
swapEitherCont' ca k = ca $ \case Left a -> k (Right a)
                                  Right a -> k (Left a)
absurd' :: Void -> a
absurd' = \case{}

inspect $ 'id === 'id'
inspect $ 'const === 'const'
inspect $ 'dup === 'dup'
inspect $ 'pair === 'pair'
inspect $ 'tripl === 'tripl'
inspect $ 'proj === 'proj'
inspect $ 'curry === 'curry'
inspect $ 'curry2 === 'curry2'
inspect $ 'unit === 'unit'
inspect $ 'contBind === 'contBind'
inspect $ 'swapEither === 'swapEither'
inspect $ 'swapEitherCont === 'swapEitherCont'
inspect $ 'absurd === 'absurd'

main :: IO ()
main = putStrLn "☺"
