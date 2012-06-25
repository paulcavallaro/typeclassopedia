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
  pure gs = ZipList [gs]
  (ZipList gs) <*> (ZipList xs) = ZipList (zipWith ($) gs xs)

{-

instance Applicative [] where
  pure x = [x]
  gs <*> xs = [ g x | g <- gs, x <- xs ]

-}

{-

instance Applicative ((->) e) where
  pure x = \_ -> x
  (<*>) g h x = g x (h x)

-}