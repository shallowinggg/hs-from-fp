{-# LANGUAGE FlexibleInstances #-}

module Functor.Exercises where

import Data.Array (Array)

-- Exercises: Be Kind

-- 1. a :: *

-- 2.
-- b :: * -> *
-- T :: * -> *

-- 3.
-- c :: * -> * -> *

-- Determine if a valid Functor can be written for the datatype provided.

-- 1. N

-- 2. Y

data BoolAndSomethingElse a
  = False' a
  | True' a

instance Functor BoolAndSomethingElse where
  fmap f (False' x) = False' $ f x
  fmap f (True' x) = True' $ f x

-- 3. Y

data BoolAndMaybeSomethingElse a
  = Falsish
  | Truish a

instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish x) = Truish $ f x

-- 4. Y
-- Mu :: (* -> *) -> *

newtype Mu f = InF {outF :: f (Mu f)}

-- 5. N
-- D :: *
data D = D (Array Word Word) Int Int

-- Rearrange the arguments to the type constructor of the datatype so the Functor instance works.

-- 1.
data Sum b a
  = First a
  | Second b

instance Functor (Sum e) where
  fmap f (First a) = First (f a)
  fmap f (Second b) = Second b

-- 2.
data Company a c b
  = DeepBlue a c
  | Something b

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

--3.
data More b a
  = L a b a
  | R b a b
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.

-- 1.
data Quant a b
  = Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor b) = Bloor $ f b

-- 2.
newtype K a b = K a

instance Functor (K a) where
  fmap _ (K a) = K a

-- 3.
newtype Flip f a b
  = Flip (f b a)
  deriving (Eq, Show)

-- should remind you of an
-- instance you've written before
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K $ f a

-- 4.

newtype EvilGoateeConst a b
  = GoatyConst b
  deriving (Show)

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst $ f b

-- 5.
newtype LiftItOut f a
  = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut $ fmap f fa

-- 6.
data Parappa f g a
  = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7.
data IgnoreOne f g a b
  = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa gb) = IgnoringSomething fa $ fmap f gb

-- 8.
data Notorious g o a t
  = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
  fmap f (Notorious go ga gt) = Notorious go ga $ fmap f gt

-- 9.
data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a la) = Cons (f a) (fmap f la)

-- 10.
data GoatLord a
  = NoGoat
  | OneGoat a
  | MoreGoats
      (GoatLord a)
      (GoatLord a)
      (GoatLord a)
  deriving (Show)

instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat $ f a
  fmap f (MoreGoats g1 g2 g3) = MoreGoats (fmap f g1) (fmap f g2) (fmap f g3)

-- 11.
data TalkToMe a
  = Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print s a) = Print s $ f a
  fmap f (Read g) = Read (f . g)
