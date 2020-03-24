{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE InstanceSigs #-}

-- JZ:TODO: I would recommand put sum in Data.Functor.Internal.Sum, and then export from Control.Effect
module Control.Effect.Close 
  where


import Data.Kind (Type)
import Data.Proxy

-- | Higher-order sums are used to combine multiple effects into a signature, typically by chaining okn the right.
data (f :+: g) (m :: Type -> Type) k
  = L (f m k)
  | R (g m k)
  deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

infixr 4 :+:

data Pos = Here|Le Pos|Ri Pos
data Res = Found Pos | NotFound | Ambiguous

type Nat = (Type -> Type) -> Type -> Type
type family Elem (e :: Nat) (p :: Nat) :: Res where
  Elem a a = 'Found 'Here
  Elem e (l :+: r) = Choose (Elem e l) (Elem e r)
  -- UndecidableInstances is caused by here.
  -- Perhaps we can find a way to remove it with decorate a Nil on the end
  -- But then we need to change the definition of f + g
  Elem _ _ = 'NotFound

type family Choose (l :: Res) (r :: Res) :: Res where
  Choose ('Found _) ('Found _) = 'Ambiguous
  Choose 'Ambiguous _ = 'Ambiguous
  Choose _ 'Ambiguous = 'Ambiguous
  Choose ('Found x) 'NotFound = 'Found ('Le x)
  Choose 'NotFound ('Found x) = 'Found ('Ri x)
  Choose 'NotFound 'NotFound = 'NotFound


class Subsume (pos :: Res) a b where
  iinj :: (Proxy pos) -> a m x -> b m x

instance Subsume ('Found 'Here) a a where
  iinj _ = id
  {-# INLINE iinj #-}

instance Subsume ('Found p) a b => Subsume ('Found ('Le p)) a (b :+: c) where
  iinj _ = L . iinj (Proxy :: Proxy ('Found p))
  {-# INLINE iinj #-}

instance Subsume ('Found p) a b => Subsume ('Found ('Ri p)) a (c :+: b) where
  iinj _ = R . iinj (Proxy :: Proxy ('Found p))
  {-# INLINE iinj #-}
