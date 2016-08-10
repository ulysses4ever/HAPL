{-# LANGUAGE KindSignatures, GADTs, TypeFamilies, TypeOperators, MultiParamTypeClasses #-}

import Prelude hiding (lookup)
import Control.Applicative (liftA2)
import Data.Array (Array, listArray)

-------------------------------------
--            Vectors
-------------------------------------
data Nat :: * where
    Z :: Nat
    S :: Nat -> Nat

data Vector :: Nat -> * -> * where 
    VNil  :: Vector Z a 
    VCons :: a -> Vector n a -> Vector (S n) a

vmap :: (a -> b) -> Vector n a -> Vector n b
vmap _ VNil        = VNil
vmap f (VCons a v) = VCons (f a) (vmap f v)

vzipWith :: (a -> b -> c) -> Vector n a -> Vector n b -> Vector n c
vzipWith _ VNil _    = VNil
vzipWith _ _    VNil = VNil
vzipWith f (VCons a va) (VCons b vb) = 
    VCons (f a b) (vzipWith f va vb)

----------------------------------------------
--            Classes: Naperian, Dim, Shapely
----------------------------------------------
class Applicative f => Naperian f where
    type Log f
    lookup :: f a -> (Log f -> a)
    table  :: (Log f -> a) -> f a
    
transpose :: (Naperian f, Naperian g) => f (g a) -> g (f a)
transpose = table . fmap table . flip . fmap lookup . lookup

class(Naperian f, Traversable f) => Dim f

class Shapely (fs :: [* -> *]) -- where::
instance Shapely '[] -- where::
instance (Dim f, Shapely fs) => Shapely (f ': fs) -- where:::

-------------------------------------
--            Hypercuboid datatype
-------------------------------------
data Hyper :: [* -> *] -> * -> * where
    Scalar :: a -> Hyper '[] a
    Prism  :: (Dim f, Shapely fs) => 
                Hyper fs (f a) -> Hyper (f ': fs) a

-- TODO: instances of Applicative, Naperian, Foldable, and Traversable...

-- TODO: make `unary` compile
--unary :: Shapely fs => (a -> b) -> (Hyper fs a -> Hyper fs b)
--unary = fmap

-------------------------------------
--            Aligning shapes
-------------------------------------
class (Shapely fs, Shapely gs) => Align fs gs where
    align :: Hyper fs a -> Hyper gs a

instance Align '[] '[] where
    align = undefined -- TODO: add definition

instance (Dim f, Align fs gs) => Align (f ': fs) (f ': gs) where
    align = undefined -- TODO: add definition

instance (Dim f, Shapely fs) => Align '[] (f ': fs) where
    align = undefined -- TODO: add definition

type family Max(fs::[* -> *]) (gs::[* -> *])::[* -> *]

-- TODO: make `binary` compile
--binary::(Max fs gs ~ hs, Align fs hs, Align gs hs) =>
--    (a -> b -> c) -> (Hyper fs a -> Hyper gs b -> Hyper hs c)
--binary f x y = liftA2 f (align x) (align y)

-------------------------------------
--            Flatten a shape
-------------------------------------
data Flat fs a where
    Flat :: Shapely fs => Array Int a -> Flat fs a

flatten    :: Shapely fs => Hyper fs a -> Flat fs a
flatten xs =  Flat (listArray (0, sizeHyper xs - 1) (elements xs))
    where sizeHyper :: Shapely fs => Hyper fs a -> Int
          sizeHyper = undefined -- TODO: add definition
          elements  :: Shapely fs => Hyper fs a -> [a]
          elements  = undefined -- TODO: add definition

