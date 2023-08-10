{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module Vect where

    import           Data.Singletons        

    import Nat    

    data Vect :: (Nat -> * -> *) where
        Empty :: Vect Z a
        Cons  :: a -> Vect m a -> Vect (S m) a

    showVect :: Show a => Vect m a -> String
    showVect Empty = ""
    showVect xs    = "["++(showVectHelper xs)++"]"
        where
            showVectHelper (Cons x Empty) = show x
            showVectHelper v = foldrVect (\x r -> (show x) ++ ","++r) "" v

    headVect :: Vect (S m) a -> a
    headVect (Cons d _) = d

    foldrVect :: (a -> b -> b) -> b -> Vect m a -> b
    foldrVect f s Empty = s
    foldrVect f s (Cons x xs) = f x (foldrVect f s xs)

    mapVect :: (a -> b) -> Vect m a -> Vect m b
    mapVect f Empty = Empty
    mapVect f (Cons x xs) = Cons (f x) (mapVect f xs)

    lengthVect :: Vect m a -> Int
    lengthVect = foldrVect (\t r -> 1 + r) 0

    zipVect :: Vect m a -> Vect m b -> Vect m (a,b)
    zipVect Empty _ = Empty
    zipVect (Cons t1 ts1) (Cons t2 ts2) = Cons (t1,t2) $ zipVect ts1 ts2

    fromList :: [a] -> Vect m a 
    fromList = undefined

    toList :: Vect m a -> [a]
    toList Empty = []
    toList (Cons t ts) = t : toList ts


