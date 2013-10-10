module MonadExercises where

import Control.Monad (join)

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

(>>>=) :: (Monad m, Functor m) => m a -> (a -> m b) -> m b
(>>>=) x g = join $ fmap g x

join' :: (Monad m) => m (m a) -> m a
join' x = x >>= id

fmap' :: (Monad m) => (a -> b) -> m a -> m b
fmap' g x = x >>= (\y ->
                    return (g y))

ap' :: (Monad m) => m (a -> b) -> m a -> m b
ap' g x = g >>= (\y ->
                  x >>= (\z ->
                          return (y z)))

sequence' :: Monad m => [m a] -> m [a]
sequence' (x:xs) = x >>= (\y ->
                           sequence' xs >>= (\z ->
                                              return $ y : z))
sequence' [] = return []

sequence'' :: Monad m => [m a] -> m [a]
sequence'' = foldr mcons (return [])
  where
    mcons p q = p >>= (\x ->
                        q >>= (\y ->
                                return $ x : y))

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
  (>>)   :: m a -> m b -> m b
  m >> n = m >>= \_ -> n

  fail   :: String -> m a

class Applicative m => Monad' m where
  (>>=) :: m a -> (a -> m b) -> m b

newtype Identity a = Identity { runIdentity :: a }

instance Monad Identity where
  return a = Identity a
  m >>= k = k (runIdentity m)
