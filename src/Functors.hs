module FunctorExercises where

data Pair a = Pair a a

instance Functor (Either e) where
  fmap _ (Left l) = Left l
  fmap g (Right r) = Right (g r)

instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)

instance Functor ((,) e) where
  fmap g (a, b) = (a, g b)

newtype Flip b a = Flip (a, b)

instance Functor ((->) e) where
  fmap g x = g . x

instance Functor (Flip e) where
  fmap g (Flip (a, b)) = Flip (g a, b)

data ITree a = Leaf (Int -> a)
             | Node [ITree a]

instance Functor ITree where
  fmap g (Leaf h) = Leaf (g . h)
  fmap g (Node ts) = Node (map (fmap g) ts)


teaser :: Functor f => (a -> b) -> f (f a) -> f (f b)
teaser g c = fmap (fmap g) c

teaser2 :: (Functor f, Functor f1) => f (a -> b) -> f (f1 a -> f1 b)
teaser2 = fmap fmap

{-
fmap :: (a -> b) -> (f a -> f b)
fmap :: ((->) a b) -> (f a -> f b)
fmap :: ((->) a b) -> ((->) (f a) (f b))
fmap :: ((->) ((->) a b) ((->) (f a) (f b)))
fmap :: ((->) (a -> b) (f a -> f b))
-}

--teaser3 :: (Functor f, Functor f1, Functor ((->) (a -> b))) => (a -> b) -> f (f1 a) -> f (f1 b)
--teaser3 = fmap fmap fmap

data NoFunctor a = NoFunctor (a -> Int)