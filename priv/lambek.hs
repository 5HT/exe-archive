{-# LANGUAGE RankNTypes, ExistentialQuantification, DeriveTraversable, PatternSynonyms, UndecidableInstances, FlexibleInstances, ViewPatterns, InstanceSigs #-}
module Free where

import Control.Monad (ap)
import Control.Applicative (empty, Alternative, (<|>))

-- Free   f a = Mu x . a + f x
-- Cofree f a = Nu x . a * f x
--   Mu < Fix < Nu
newtype Fix f = Fix (f (Fix f))

unfix :: Fix f -> f (Fix f)
unfix (Fix f) = f

cata :: Functor f => (f b -> b) -> Fix f -> b
cata alg = alg . fmap (cata alg) . unfix

ana :: Functor f => (a -> f a) -> a -> Fix f
ana coalg = Fix . fmap (ana coalg) . coalg

para :: Functor f => (f (Fix f, c) -> c) -> Fix f -> c
para psi = snd . cata (\m -> (Fix (fmap fst m), psi m))

apo :: Functor f => (b -> f (Either (Fix f) b)) -> b -> Fix f
apo coalg = Fix . fmap(either id (apo coalg)) . coalg

zygo :: Functor f => (f a -> a) -> (f (a, b) -> b) -> Fix f -> b
zygo g f = snd . cata (\x -> (g $ fmap fst x, f x))

prepro :: Functor f => (f (Fix f) -> f (Fix f)) -> (f b -> b) -> Fix f -> b
prepro e f = f . fmap (prepro e f . cata (Fix . e)) . unfix

postpro :: Functor f => (f (Fix f) -> f (Fix f)) -> (a -> f a) -> a -> Fix f
postpro e g = Fix . fmap (ana (e . unfix) . postpro e g) . g

gapo :: Functor f => (a -> f a) -> (b -> f (Either a b)) -> b -> Fix f
gapo f g = Fix . fmap (either (ana f) (gapo f g)) . g

hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo f g = h where h = f . fmap h . g

dyna :: Functor f => (f (Cofree f a) -> a) -> (c -> f c) -> c -> a
dyna fa fc = extract . hylo (\fb ->  Cofree(Fix(CoBindF (fa fb) (fmap uncofree fb)))) fc

histo :: Functor f => (f (Cofree f a) -> a) -> Fix f -> a
histo f = extract . cata (\x -> Cofree(Fix (CoBindF (f x) (fmap uncofree x))))

futu :: Functor f => (a -> f (Free f a)) -> a -> Fix f
futu f = Fix . fmap w . f where  
    w (Free(Fix(ReturnF a))) = futu f a
    w (Free(Fix(BindF g))) = Fix (fmap (\x -> w(Free x)) g)

data CofreeF f a r = CoBindF a (f r) deriving (Functor, Foldable, Traversable)

data FreeF f a r = ReturnF a | BindF (f r) deriving (Functor, Foldable, Traversable)

newtype Free f a = Free(Fix(FreeF f a))

newtype Cofree f a = Cofree(Fix(CofreeF f a))

unfree :: Free f a -> Fix (FreeF f a)
unfree (Free v) = v

uncofree :: Cofree f a -> Fix (CofreeF f a)
uncofree (Cofree v) = v

extract :: Cofree f a -> a
extract (Cofree(Fix(CoBindF a _))) = a

---type MendlerAlgebra f c = forall a . (a -> c) -> f a -> c

--mcata :: (forall a . (a -> c) -> f a -> c)  -> Fix f -> c
mcata phi = phi (mcata phi) . unfix

--mcata :: (forall y. (y -> c) -> f y -> c) -> Fix f -> c
--mcata2 psi t = psi (\x -> mcata psi x) (unfix t)

instance Functor f => Functor (Cofree f) where
    fmap :: (a -> b) -> Cofree f a -> Cofree f b
    fmap f = Cofree . go . uncofree where
        go (Fix (CoBindF a as)) = Fix (CoBindF (f a) (fmap go as))

instance Alternative f => Monad (Cofree f) where
    return a = Cofree (Fix (CoBindF a empty))
    (Cofree(Fix(CoBindF a m))) >>= f = case f a of
        (Cofree(Fix(CoBindF b n))) -> Cofree(Fix(CoBindF b (fmap uncofree ((fmap Cofree n) <|> (fmap ((>>= f) . Cofree) m)))))   

instance Alternative f => Applicative(Cofree f) where
    pure = return
    (<*>) = ap  

liftF :: Functor f => f a -> Free f a
liftF c = Free (Fix (BindF $ fmap (unfree . Free . Fix . ReturnF) c))

instance Functor f => Functor (Free f) where
    fmap :: (a -> b) -> Free f a -> Free f b
    fmap f = Free . cata go . unfree where
        go (ReturnF a) = Fix (ReturnF (f a))
        go (BindF   a) = Fix (BindF a)

instance Functor f => Applicative(Free f) where
    pure = return
    (<*>) = ap    

instance Functor f => Monad (Free f) where
    return a = Free (Fix (ReturnF a))
    x >>= f = Free $ go $ unfree x where
                go (Fix (ReturnF a)) = unfree $ f a
                go (Fix (BindF a)) = Fix . BindF $ fmap go a
  
retract :: Monad f => Free f b -> f b
retract = go . unfree where
    go (Fix(ReturnF a)) = return a
    go (Fix(BindF as)) = as >>= go 
