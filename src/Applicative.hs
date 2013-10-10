module ApplicativeExercises where

import Control.Applicative (Applicative, pure, (<*>))

{-

instance Applicative Maybe where
  pure x = Just x
  (<*>) (Just g) (Just x) = Just (g x)
  (<*>) _ _ = Nothing

-}

newtype ZipList a = ZipList { getZipList :: [a] }

instance Functor ZipList where
  fmap g (ZipList gs) = ZipList (map g gs)

instance Applicative ZipList where
  pure gs = ZipList (repeat gs)
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

{-

instance Applicative [] where
  pure x = [x]
  gs <*> xs = [ g x | g <- gs, x <- xs ]

-}

anon = pure (+) <*> [2,3,4] <*> pure 4

{-

instance Applicative ((->) e) where
  pure x = \_ -> x
  (<*>) g h x = g x (h x)

-}

{-

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

Applicative Laws:

-- Identity
pure id <*> v = v
-- Homomorphism
pure f <*> pure x = pure (f x)
-- Interchange
u <*> pure y = pure ($ y) <*> u
-- Composition
u <*> (v <*> w) = pure (.) <*> u <*> v <*> w
-- Relation with Functor
fmap g x = pure g <*> x

-}

{-
(Tricky) One might imagine a variant of the interchange law that says something
about applying a pure function to an effectful argument. Using the above laws,
prove that

pure f <*> x = pure (flip ($)) <*> x <*> pure f

f is pure, i.e., f :: a -> b
x is in context, i.e.,  x :: g a
pure f <*> x :: g b
pure f :: g (a -> b)
By
pure (flip ($)) <*> x :: g ((a -> c) -> c)
pure (flip ($)) :: g (a -> (a -> b) -> b)
<*> :: g (a -> b) -> g a -> g b
pure (flip ($)) <*> x :: g ((a -> b) -> b)
pure (flip ($)) <*> x <*> pure f :: g b

Not sure if it's a proof, but following the type system by hand
checks out?
-}


class Functor f => Monoidal f where
  unit :: f ()
  (**) :: f a -> f b -> f (a, b)

{-
Monoidal/Applicative laws

Naturality:
fmap (g *** h) (u ** v) = fmap g u ** fmap h v
Left Identity:
unit ** v = v
Right Identity:
u ** unit = u
Associativity:
u ** (v ** w) = (u ** v) ** w

-}
