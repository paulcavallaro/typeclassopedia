module TrivialMonad where

data W a = W a deriving Show

return :: a -> W a
return x = W x

bind :: (a -> W b) -> (W a -> W b)
bind f (W x) = f x

g :: Int -> W Int -> W Int
-- g x (W y) = W (x + y)
g x = bind (\z -> W (z + x))

h :: W Int -> W Int -> W Int
--h (W x) (W y) = W (x + y)
h u v = bind (\x ->
               bind (\y -> W (x + y)) u) v
