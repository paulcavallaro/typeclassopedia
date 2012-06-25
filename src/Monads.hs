module MonadExercises where

{-

instance Monad Maybe where
  return = Just
--(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
  Just x >>= g = g x
  Nothing >>= _ = Nothing

instance Monad [] where
  return = []
--(>>=) :: [a] -> (a -> [b]) -> [b]
  xs >>= g = map g . concat

-}

instance Monad ((->) e) where
  return = const
--(>>=) :: (e -> a) -> (a -> (e -> b)) -> (e -> b)
  (>>=) x g y = g (x y) y

data Free f a = Var a
              | Node (f (Free f a))

instance (Functor f) => Functor (Free f) where
  fmap g (Var x) = Var $ g x
  fmap g (Node x) = Node (fmap (fmap g) x)
