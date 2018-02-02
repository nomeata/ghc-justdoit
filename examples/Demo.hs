{-# OPTIONS_GHC -fplugin=GHC.JustDoIt.Plugin #-}
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin #-}
{-# LANGUAGE TemplateHaskell, LambdaCase, EmptyCase #-}
import GHC.JustDoIt
import Test.Inspection (inspect, (===), (==-))

import Prelude hiding (id, flip, const, curry)

-- Some auxillary definitions

data Unit = Unit
data Void
data MyLargeSum a b c d e = MkA a | MkB b | MkC c | MkD d | MkE e
newtype Id a = Id a
data Result a b = Failure a | Success b
newtype ErrRead r e a = ErrRead { unErrRead :: r -> Result e a }

-- All these functions have basically one sensible implementation.
-- With GHC.JustDoIt, we don’t have to write them.

id :: a -> a
id = justDoIt

const :: a -> b -> a
const = (…)

flip :: (a -> b -> c) -> (b -> a -> c)
flip = (…)

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

curryFlip :: ((a,b) -> c) -> b -> a -> c
curryFlip = (…)

contBind :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
contBind = (…)

contAp :: (((a -> b) -> r) -> r) -> ((a -> r) -> r) -> ((b -> r) -> r)
contAp = (…)

errReadBind :: (r -> Either e a) -> (a -> (r -> Either e b)) -> (r -> Either e b)
errReadBind = (…)

errReadBindTup :: (r -> Either e a) -> (a -> (r -> Either e b)) -> (r -> Either e (a,b))
errReadBindTup = (…)

errReadAp :: (r -> Either e (a -> b)) -> (r -> Either e a) -> (r -> Either e b)
errReadAp = (…)

errReadBindTup2 :: ErrRead r e a -> (a -> ErrRead r e b) -> ErrRead r e (a,b)
errReadBindTup2 = (…)

unit :: Unit
unit = (…)

swapEither :: Either a b -> Either b a
swapEither = (…)

swapEitherCont :: (((Either a b) -> r) -> r) -> (((Either b a) -> r) -> r)
swapEitherCont = (…)

randomCrap :: (a -> b) -> (a,c,d) -> (d,b,b)
randomCrap = (…)

absurd :: Void -> a
absurd = (…)

convert :: MyLargeSum a b c d e -> Either a (Either b (Either c (Either d e)))
convert = (…)

mapId :: (a -> b) -> Id a -> Id b
mapId = (…)

-- Just for comparison, here are the implementations that you might write by
-- hand

id' x = x
const' x _= x
flip' f a b = f b a
dup' x = (x,x)
pair' x y = (x,y)
tripl' x y z = (x,y,z)
proj' (_,_,c,_) = c
curry' f a b = f (a,b)
curryFlip' f a b = f (b,a)
unit' = Unit
mapId' f (Id x) = Id (f x)
contBind' :: ((a -> r) -> r) -> (a -> ((b -> r) -> r)) -> ((b -> r) -> r)
contBind' ca cb k = ca (\a -> cb a k)
swapEither' (Left a) = (Right a)
swapEither' (Right a) = (Left a)
swapEitherCont' :: (((Either a b) -> r) -> r) -> (((Either b a) -> r) -> r)
swapEitherCont' ca k = ca $ \case Left a -> k (Right a)
                                  Right a -> k (Left a)
absurd' :: Void -> a
absurd' = \case{}
errReadBind' m1 m2 r = case m1 r of Left e -> Left e
                                    Right a -> m2 a r
errReadBindTup' m1 m2 r = case m1 r of
    Left e -> Left e
    Right a -> case m2 a r of Left e -> Left e
                              Right b -> Right (a,b)

errReadBindTup2' :: ErrRead r e a -> (a -> ErrRead r e b) -> ErrRead r e (a,b)
errReadBindTup2' m1 m2 = ErrRead $ \r -> case unErrRead m1 r of
    Failure e -> Failure e
    Success a -> case unErrRead (m2 a) r of Failure e -> Failure e
                                            Success b -> Success (a,b)

-- Here are functions where we do not infer the expected code, due to the order
-- things are looked at.
errReadAp' m1 m2 r = case m2 r of Left e -> case m1 r of Left e -> Left e
                                                         Right _ -> Left e
                                  Right x -> case m1 r of Left e -> Left e
                                                          Right f -> Right (f x)
contAp' :: (((a -> b) -> r) -> r) -> ((a -> r) -> r) -> ((b -> r) -> r)
contAp' ca cb k = cb (\x -> ca (\f -> k (f x)))

-- And here we use inspection-testing to check that these are indeed the
-- definitions that GHC.JustDoIt created for us.

inspect $ 'id              === 'id'
inspect $ 'const           === 'const'
inspect $ 'flip            === 'flip'
inspect $ 'dup             === 'dup'
inspect $ 'pair            === 'pair'
inspect $ 'tripl           === 'tripl'
inspect $ 'proj            === 'proj'
inspect $ 'curry           === 'curry'
inspect $ 'curryFlip       === 'curryFlip'
inspect $ 'unit            === 'unit'
inspect $ 'contBind        === 'contBind'
inspect $ 'contAp          === 'contAp'
inspect $ 'swapEither      === 'swapEither'
inspect $ 'swapEitherCont  === 'swapEitherCont'
inspect $ 'absurd          === 'absurd'
inspect $ 'mapId           === 'mapId'
inspect $ 'errReadBind     === 'errReadBind'
inspect $ 'errReadBindTup  === 'errReadBindTup'
inspect $ 'errReadBindTup2 ==- 'errReadBindTup2' -- type variable order differences
inspect $ 'errReadAp       === 'errReadAp'

main :: IO ()
main = putStrLn "☺"
